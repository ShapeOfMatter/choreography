{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Choreography.EasySyntaxTree
where

import Data.Maybe (catMaybes)
import Polysemy (Members, Sem)
import Polysemy.Error (Error)
import Polysemy.Reader (ask, Reader, runReader)
import Polysemy.State (evalState, gets, modify, State)
import Polysemy.Tagged (tag, Tagged, untag)

import qualified Choreography.AbstractSyntaxTree as AST
import Choreography.Party (Party(..))
import Utils (LineNo, runFailOr, throwLn)


type Variable = String

data Algebra = Literal Bool
             | Var Variable
             | Xor Algebra Algebra
             | And Algebra Algebra
             | Not Algebra
             deriving (Show)

data Expression = Compute Algebra
                | Secret
                | Flip
                | Send Algebra
                | Output Algebra
                | Annotated Party Expression
                deriving (Show)

type Program = [Maybe (Variable, Expression)]

compileSem :: forall lnTag r.
              (Members '[Error String] r) =>
              Program -> Sem r AST.Program
compileSem = (catMaybes <$>) . evalState [] . sequence . (compLine <$> [1..] <*>)
  where returnVar var party val = do lineNo <- tag @lnTag ask
                                     modify ((var, party):)
                                     return $ Just (lineNo, AST.Variable party var, val)

        compLine :: LineNo -> Maybe (Variable, Expression) -> Sem (State [(Variable, Party)]':r) (Maybe (LineNo, AST.Variable, AST.Expression))
        compLine _ Nothing = return Nothing
        compLine lineNo (Just (variable, e)) = runReader lineNo . untag @lnTag $ do
            party <- expParty e >>= maybe (throwLn "Can't infer party location.") return
            e' <- compExp party e
            returnVar variable party e'

        compExp :: Party -> Expression -> Sem (Tagged lnTag (Reader LineNo) ': State [(Variable, Party)] ': r) AST.Expression
        compExp p (Compute alg) = AST.Compute <$> compAlg p alg
        compExp p Secret = return $ AST.Secret p
        compExp p Flip = return $ AST.Flip p
        compExp p (Send alg) = do p1 <- algParty alg >>= maybe (throwLn "Can't infer sender.") return
                                  alg' <- compAlg p1 alg
                                  return $ AST.Send p alg'
        compExp p (Output alg) = AST.Output <$> compAlg p alg
        compExp p (Annotated _ e) = compExp p e

        compAlg :: Party -> Algebra -> Sem (Tagged lnTag (Reader LineNo) ': State [(Variable, Party)] ': r) AST.Algebra
        compAlg p (Literal b) = return $ AST.Literal p b
        compAlg p (Var var) = return $ AST.Var $ AST.Variable p var
        compAlg p (Xor a1 a2) = do a1' <- compAlg p a1
                                   a2' <- compAlg p a2
                                   return $ AST.Xor a1' a2'
        compAlg p (And a1 a2) = do a1' <- compAlg p a1
                                   a2' <- compAlg p a2
                                   return $ AST.Xor a1' a2'
        compAlg p (Not alg) = AST.Not <$> compAlg p alg

expParty :: forall lnTag r.
            (Members '[Tagged lnTag (Reader LineNo), Error String, State [(Variable, Party)]] r) =>
            Expression -> Sem r (Maybe Party)
expParty (Compute alg) = algParty alg
expParty Secret = return Nothing
expParty Flip = return Nothing
expParty (Send _) = return Nothing
expParty (Output alg) = algParty alg
expParty (Annotated p2 e) = runFailOr (Just p2) $ do Just p1 <- expParty e
                                                     if p1 == p2
                                                       then return $ Just p1
                                                       else throwLn $ "Expression at " ++ show p1 ++ " annotated for other party."

algParty :: forall lnTag r.
            (Members '[Tagged lnTag (Reader LineNo), Error String, State [(Variable, Party)]] r) =>
            Algebra -> Sem r (Maybe Party)
algParty (Literal _) = return Nothing
algParty (Var var) = gets (lookup var) >>= maybe (throwLn $ "Unbound variable " ++ var ++ ".") (return . Just)
algParty (Xor a1 a2) = runFailOr Nothing $ do Just p1 <- algParty a1
                                              Just p2 <- algParty a2
                                              if p1 == p2
                                                then return $ Just p1
                                                else throwLn "Computation over values in different parties in XOR clause."
algParty (And a1 a2) = runFailOr Nothing $ do Just p1 <- algParty a1
                                              Just p2 <- algParty a2
                                              if p1 == p2
                                                then return $ Just p1
                                                else throwLn "Computation over values in different parties in AND clause."
algParty (Not alg) = algParty alg
