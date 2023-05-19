module Choreography.Validate
where

import Control.Arrow (ArrowChoice(left))
import Data.Map.Strict ((!?), insertWith, Map)
import Polysemy (Members, run, Sem)
import Polysemy.Error (Error, runError, throw)
import Polysemy.State (evalState, gets, modify, State)

import Choreography.AbstractSyntaxTree
import Choreography.Parser (Sourced)
import Choreography.Party
import Utils ((<$$>), Pretty, pretty)


validate :: Map Variable PartySet -> Program Sourced -> Either String (Program Located)
validate context = left pretty . run . runError . evalState context . traverse validateStatement

validateStatement :: forall r.
                     (Members '[Error (Sourced String), State (Map Variable PartySet)] r) =>
                     Sourced (Statement Sourced) -> Sem r (Located (Statement Located ))
validateStatement (sourcePos, stmnt) = case stmnt of
  Compute fvar@(_, v) falg -> do (ps, alg) <- validateAlg falg
                                 bindToParties v ps
                                 locateLine ps $ Compute (locate ps fvar) alg
  Secret fvar@(_, v) fp@(_, p) -> do let ps = singleton p
                                     bindToParties v ps
                                     locateLine ps $ Secret (locate ps fvar) (locate ps fp)
  Flip fvar@(_, v) fp@(_, p) -> do let ps = singleton p
                                   bindToParties v ps
                                   locateLine ps $ Flip (locate ps fvar) (locate ps fp)
  Send fps@(_, ps2) fvar@(_, v) -> do ps1 <- lookupVar fvar
                                      bindToParties v ps2
                                      locateLine (ps1 `union` ps2) $ Send (locate ps2 fps) (locate ps1 fvar)
  Oblivious fvar@(_, v) fps@(_, ps2) fob -> do (ps1, body) <- validateObliv ps2 fob
                                               bindToParties v ps2
                                               locateLine (ps1 `union` ps2) $ Oblivious (locate ps2 fvar) (locate ps2 fps) body
  Output fvar -> do ps <- lookupVar fvar
                    locateLine ps $ Output (locate ps fvar)
  where locateLine :: PartySet -> Statement Located -> Sem r (Located (Statement Located))
        locateLine ps st = return $ locate ps (sourcePos, st)

locate :: PartySet -> Sourced a -> Located a
locate ps (source, a) = (Location{source, lowners=ps}, a)

bindToParties :: forall k v r.
                 (Members '[State (Map k v)] r,
                  Ord k,
                  Semigroup v) =>
                 k -> v -> Sem r ()
bindToParties = modify <$$> insertWith (<>)

lookupVar :: forall k v r.
             (Members '[State (Map k v), Error (Sourced String)] r,
              Ord k,
              Pretty k) =>
             Sourced k -> Sem r v
lookupVar (source, var) = do mps <- gets (!? var)
                             maybe (throw (source, "Variable " ++ pretty var ++ " is not in scope.")) return mps

validateObliv :: forall r.
                 (Members '[Error (Sourced String), State (Map Variable PartySet)] r) =>
                 PartySet -> Sourced (ObvBody Sourced) -> Sem r (PartySet, Located (ObvBody Located))
validateObliv ps2 (source, ObvBody fBranch tBranch choice)
  = do psc <- lookupVar choice
       (psf, fBranch') <- validateBranch fBranch
       (pst, tBranch') <- validateBranch tBranch
       let mps1 = psf `intersect` pst
       ps1 <- maybe (throw (source, "Nobody can obliviosly serve both branches.")) return mps1
       let participants = ps2 `union` ps1
       if ps2 `isSubsetOf` psc
         then return (ps1, locate participants (source, ObvBody fBranch' tBranch' (locate participants choice)))
         else throw (source, "Not all of " ++ pretty ps2 ++ " can obliviously choose by " ++ pretty choice ++ ".")
  where validateBranch :: Sourced (ObvChoice Sourced) -> Sem r (PartySet, Located (ObvChoice Located))
        validateBranch (src, ObvLeaf var) = do ps <- lookupVar (src, var)
                                               return (ps, locate ps (src, ObvLeaf var))
        validateBranch (src, ObvBranch body) = do (ps, (location, body')) <- validateObliv ps2 (src, body)
                                                  return (ps, (location{source=src}, ObvBranch body'))

validateAlg :: forall r.
               (Members '[Error (Sourced String), State (Map Variable PartySet)] r) =>
               Sourced (Algebra Sourced) -> Sem r (PartySet, Located (Algebra Located))
validateAlg (src, algebra) = case algebra of
  Literal fbit -> return (top, locate top (src, Literal $ locate top fbit))
  Var fvar -> do ps <- lookupVar fvar
                 return (ps, (Location{source=src, lowners=ps}, Var $ locate ps fvar))
  Xor fa1 fa2 -> do (ps1, alg1) <- validateAlg fa1
                    (ps2, alg2) <- validateAlg fa2
                    ps <- ps1 `inter` ps2
                    return (ps, (Location{source=src, lowners=ps}, Xor alg1 alg2))
  And fa1 fa2 -> do (ps1, alg1) <- validateAlg fa1
                    (ps2, alg2) <- validateAlg fa2
                    ps <- ps1 `inter` ps2
                    return (ps, (Location{source=src, lowners=ps}, And alg1 alg2))
  Not falg -> do (ps, alg@(location, _)) <- validateAlg falg
                 return (ps, (location{source=src}, Not alg))
  where ps1 `inter` ps2 = maybe (throw (src, "Nobody has all the pieces to compute " ++ pretty algebra ++ ".")) return $ ps1 `intersect` ps2


