module Choreography.CoreSemantics
where

import Data.Map.Strict ((!?), empty, insert, Map, singleton)
import Polysemy (Members, run, Sem)
import Polysemy.Error (Error, runError)
import Polysemy.Reader (ask, Reader, runReader)
import Polysemy.State (evalState, get, modify, State)
import Polysemy.Tagged (Tagged, untag)
import Polysemy.Writer (runWriter, tell, Writer)

import Choreography.Party (Party(..))
import Choreography.AbstractSyntaxTree
import Utils (LineNo, throwLn)

class NS ns where  -- There's got to be a better way to write this.
  namespace :: ns -> Map Variable Bool

type NameSpace = Map Variable Bool
instance NS (Map Variable Bool) where namespace = id

newtype Inputs = Inputs NameSpace deriving (Eq, Monoid, Semigroup, Show)
instance NS Inputs where namespace (Inputs ns) = ns

newtype Tapes = Tapes NameSpace deriving (Eq, Monoid, Semigroup, Show)
instance NS Tapes where namespace (Tapes ns) = ns

newtype Outputs = Outputs NameSpace deriving (Eq, Monoid, Semigroup, Show)
instance NS Outputs where namespace (Outputs ns) = ns

newtype Views = Views NameSpace deriving (Eq, Monoid, Semigroup, Show)
instance NS Views where namespace (Views ns) = ns

asks :: forall ns k r.
        (NS ns,
         Members '[Reader ns
                  ,Error String
                  ,Tagged k (Reader LineNo)] r) =>
        Variable -> Sem r Bool
asks var = do ns <- namespace <$> ask @ns
              let mVal = ns !? var
              case mVal of Just val -> return val
                           Nothing -> throwLn $ "Could not find support for " ++ variable var ++ "."

gets :: forall ns k r.
        (NS ns,
         Members '[State ns
                  ,Error String
                  ,Tagged k (Reader LineNo)] r) =>
        Variable -> Sem r Bool
gets var = do ns <- namespace <$> get @ns
              let mVal = ns !? var
              case mVal of Just val -> return val
                           Nothing -> throwLn $ "Could not find binding for " ++ variable var ++ "."

semantics :: forall r.
             (Members '[
               Reader Inputs,
               Reader Tapes,
               Writer Outputs,
               Writer Views,
               State NameSpace,
               Error String
               ] r) =>
             Program -> Sem r ()
semantics [] = return ()
semantics ((lineno, var@Variable{party, variable}, e):cont) =
  do () <- runReader lineno $ untag $ expSemantics e
     semantics cont
  where
    expSemantics (Compute alg) = do
        (val, p) <- algSemantics alg
        if party == p
          then modify $ insert var val
          else throwLn $ "Computation for "
                       ++ variable ++ " is not in " ++ show party ++ "'s namespace."
    expSemantics (Secret p) = if party == p
        then do (val :: Bool) <- asks @Inputs var
                modify $ insert var val
        else throwLn "Party-mismatched secret binding."
    expSemantics (Flip p) = if party == p
        then do (val :: Bool) <- asks @Tapes var
                modify $ insert var val
                tell . Views $ singleton var val
        else throwLn "Party-mismatched flip binding."
    expSemantics (Send p2 alg) = if party == p2
        then do (val, _) <- algSemantics alg
                modify $ insert var val
                tell . Views $ singleton var val
        else throwLn "Party-mismatched in send-receipient binding."
    expSemantics (Output alg) = do
        (val, p) <- algSemantics alg
        if party == p
          then do modify $ insert var val
                  tell . Outputs $ singleton var val
          else throwLn $ "Output "
                       ++ variable ++ " is not in " ++ show party ++ "'s namespace."

algSemantics :: forall k r.
                (Members '[
                  Reader Inputs,
                  Reader Tapes,
                  Writer Outputs,
                  Writer Views,
                  State NameSpace,
                  Error String,
                  Tagged k (Reader LineNo)
                  ] r) =>
                Algebra -> Sem r (Bool, Party)
algSemantics (Literal p b) = return (b, p)
algSemantics (Var var@Variable{party, variable=_}) = do val <- gets var
                                                        return (val, party)
algSemantics (Xor a1 a2) = do (v1, p1) <- algSemantics a1
                              (v2, p2) <- algSemantics a2
                              if p1 == p2
                                then return (v1 /= v2, p1)
                                else throwLn $ "Xor-term's arguments are in different parties namespaces"
                                               ++ " (Left: " ++ show p1 ++ ", Right: " ++ show p2 ++ ")."
algSemantics (And a1 a2) = do (v1, p1) <- algSemantics a1
                              (v2, p2) <- algSemantics a2
                              if p1 == p2
                                then return (v1 && v2, p1)
                                else throwLn $ "Xor-term's arguments are in different parties namespaces"
                                               ++ " (Left: " ++ show p1 ++ ", Right: " ++ show p2 ++ ")."
algSemantics (Not alg) = do (val, party) <- algSemantics alg
                            return (not val, party)

deterministicEvaluation :: Program -> Inputs -> Tapes -> Either String (Outputs, Views)
deterministicEvaluation p is ts = do
  (views, (outputs, ())) <- run . runError
                                . evalState empty
                                . runWriter @Views . runWriter @Outputs
                                . runReader ts . runReader is
                                $ semantics p
  return (outputs, views)

