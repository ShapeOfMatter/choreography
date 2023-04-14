module Choreography.Semantics
where

import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.Map.Strict ((!?), empty, insert, Map, singleton, unionWith, toList)
import Data.Maybe (fromMaybe, fromJust)
import Polysemy (Members, run, Sem)
import Polysemy.Reader (asks, Reader, runReader)
import Polysemy.State (gets, modify, runState, State)
import Polysemy.Writer (runWriter, tell, Writer)

import Choreography.Party (intersect, Party(..), PartySet(..))
import Choreography.AbstractSyntaxTree
import Utils ((<$$>), Pretty, pretty)

class NS ns a | ns -> a where
  find :: ns -> Variable -> Maybe a
  bind :: Variable -> a -> ns -> ns

newtype VarContext = VarContext { varContextMap :: Map Variable (PartySet, Bool) } deriving (Eq, Monoid, Semigroup, Show)
instance NS VarContext (PartySet, Bool) where
  find = (!?) . varContextMap
  bind v a = VarContext . insert v a . varContextMap
instance Pretty VarContext where
  pretty (VarContext vcm) = unlines $ (\(var, (ps, b))
      -> "  " ++ pretty var ++ ": " ++ pretty b ++ " @ " ++ pretty ps
    )  <$> toList vcm

newtype Inputs = Inputs { inputsMap :: Map Variable Bool } deriving (Eq, Monoid, Semigroup, Show)
instance NS Inputs Bool where
  find = (!?) . inputsMap
  bind v a = Inputs . insert v a . inputsMap

newtype Tapes = Tapes { tapesMap :: Map Variable Bool } deriving (Eq, Monoid, Semigroup, Show)
instance NS Tapes Bool where
  find = (!?) . tapesMap
  bind v a = Tapes . insert v a . tapesMap

newtype Outputs = Outputs { outputsMap :: Map Party (Map Variable Bool) } deriving (Eq, Show)
instance Semigroup Outputs where
  Outputs om1 <> Outputs om2 = Outputs $ unionWith (<>) om1 om2
instance Monoid Outputs where mempty = Outputs empty

newtype Views = Views { viewsMap :: Map Party (Map Variable Bool) } deriving (Eq, Show)
instance Semigroup Views where
  Views om1 <> Views om2 = Views $ unionWith (<>) om1 om2
instance Monoid Views where mempty = Views empty

{-throwLn :: forall l a r.
           (Members '[Error String] r
           ,Proper l) =>
           l String -> Sem r a
throwLn err = do ln <- tag $ ask @SourcePos
                 throw $ "[Line " ++ show ln ++ "] " ++ err-}

-- Unsafe Lookup
uslkup :: forall ns f a.
          (NS ns a
          ,Proper f) =>
          f Variable -> ns -> a
fv `uslkup` ns = fromMaybe
  (error $ "Free variable " ++ variable (value fv) ++ " appeared during evaluation.")
  (ns `find` value fv)

{-asks :: forall ns f a r.
        (NS ns,
         Proper f,
         Members '[Reader ns] r) =>
        f Variable -> Sem r a
asks var = asks >>= (`uslkup` var)

gets :: forall ns f r.
        (NS ns,
         Proper f, Functor f,
         Members '[State ns
                  ,Error (f String)] r) =>
        f Variable -> Sem r Bool
gets var = do ns <- namespace <$> get @ns
              snd <$> ns `uslkup` var-}

semantics :: forall f r.
             (Members '[
               Reader Inputs,
               Reader Tapes,
               Writer Outputs,
               Writer Views,
               State VarContext
               ] r,
              Proper f, Functor f) =>
             Program f -> Sem r ()
semantics = traverse_ stmtSemantics

stmtSemantics :: forall f r.
                 (Members '[
                   Reader Inputs,
                   Reader Tapes,
                   Writer Outputs,
                   Writer Views,
                   State VarContext
                   ] r,
                  Proper f, Functor f) =>
                 f (Statement f) -> Sem r ()
stmtSemantics fs = case value fs of
    Compute var falg -> do val <- algSemantics (value falg)
                           modify $ bind (value var) val
    Secret var _ -> do val <- asks @Inputs $ uslkup var
                       modify $ bind (value var) (owners var, val)
    Flip var _ -> do val <- asks @Tapes $ uslkup var
                     modify $ bind (value var) (owners var, val)
                     recordViews (value var) val (owners var)
    Send p2s var -> do (p1s, val) <- gets $ uslkup var
                       recordViews (value var) val (value p2s)
                       modify $ bind (value var) (p1s <> value p2s, val)
    Output var -> do (ps, val) <- gets $ uslkup var
                     if null $ parties ps
                     then error "Not implemented: Output at partyset Top!"
                     else traverse_ (\party -> tell $ Outputs $ singleton party (singleton (value var) val)) $ parties ps
    Oblivious var p2s vc (vt, vf) -> do (_, valc) <- gets $ uslkup vc
                                        let vSend = bool vf vt valc
                                        (_, val) <- gets $ uslkup vSend
                                        modify $ bind (value var) (value p2s, val)
                                        recordViews (value var) val (value p2s)
  where recordViews :: Variable -> Bool -> PartySet -> Sem r ()
        recordViews var b = traverse_ (\party -> tell
            Views{viewsMap = singleton party $ singleton var b}
          ) . parties
    {-
    Send p2 alg -> if party == p2
        then do (val, _) <- algSemantics alg
                modify $ insert var val
                tell . Views $ singleton var val
        else throwLn "Party-mismatched in send-receipient binding."
    Output alg -> do
        (val, p) <- algSemantics alg
        if party == p
          then do modify $ insert var val
                  tell . Outputs $ singleton var val
          else throwLn $ "Output "
                       ++ variable ++ " is not in " ++ show party ++ "'s namespace."
  where bindVar :: f Variable -> (PartySet, Bool) -> Sem r ()
        bindVar var evaluated = modify $ bind (value var) evaluated-}

algSemantics :: forall f r.
                 (Members '[
                   Reader Inputs,
                   Reader Tapes,
                   Writer Outputs,
                   Writer Views,
                   State VarContext
                   ] r,
                  Proper f, Functor f) =>
                Algebra f -> Sem r (PartySet, Bool)
algSemantics (Literal b) = return (owners b, bit $ value b)
algSemantics (Var var) = gets $ uslkup var
algSemantics (Xor a1 a2) = do (ps1, v1) <- algSemantics $ value a1
                              (ps2, v2) <- algSemantics $ value a2
                              return (fromJust $ ps1 `intersect` ps2, v1 /= v2)
algSemantics (And a1 a2) = do (ps1, v1) <- algSemantics $ value a1
                              (ps2, v2) <- algSemantics $ value a2
                              return (fromJust $ ps1 `intersect` ps2, v1 && v2)
algSemantics (Not alg) = do not <$$> algSemantics (value alg)

deterministicEvaluation' :: (Proper f, Functor f) =>
                            Program f -> Inputs -> Tapes -> (VarContext, (Outputs, Views))
deterministicEvaluation' p is ts =
  let (vc, (views, (outputs, ()))) = run . runState (VarContext empty)
                                   . runWriter @Views . runWriter @Outputs
                                   . runReader ts . runReader is
                                   $ semantics p
  in (vc, (outputs, views))

deterministicEvaluation :: (Proper f, Functor f) =>
                           Program f -> Inputs -> Tapes -> (Outputs, Views)
deterministicEvaluation p is ts =
  let (_, ret) = deterministicEvaluation' p is ts
  in ret
