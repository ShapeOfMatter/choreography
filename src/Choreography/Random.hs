module Choreography.Random where

import Control.Monad (when, join)
import Data.Bool (bool)
import Data.Functor.Identity (Identity(Identity), runIdentity)
import Data.List.NonEmpty (NonEmpty((:|)), cons, nonEmpty, toList, fromList)
import Data.Map (keys)
import qualified Data.Map as Map
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Set as S
import Numeric.Natural (Natural)
import Polysemy (Members, run, Sem)
import Polysemy.Random (attrit, manyOf, oneOf, Random, random, randomR, runRandom, weighted, bernoulli, shuffle)
import Polysemy.Reader (asks, local, Reader, runReader)
import Polysemy.State (evalState, gets, State)
import Polysemy.Writer (runWriter, Writer)
import qualified Polysemy.Writer as W -- so we don't use the wrong tell by accident.
import Test.QuickCheck (Gen, getSize, Arbitrary (arbitrary))
import qualified Text.Parsec.Pos as Pos
import System.Random (mkStdGen)

import Choreography.AbstractSyntaxTree
import Choreography.Functors (antimap')
import Choreography.Party (corrupt, honest, intersect, isElementOf, isSubsetOf, Party, singleton, PartySet (Parties), top)
import Choreography.Validate (bindToParties, validateAlg, ValidationState (variables))
import Utils (squareRoot, failOnError)

iii :: a -> Identity a
iii = Identity
jjj :: Identity a -> a
jjj = runIdentity

type ProgramBuilder r = Members '[ State ValidationState
                                 , Random
                                 , Reader ProgramSize
                                 , Writer (Program Identity)
                                 ] r

data BodyRatios = BodyRatios { compute :: Natural
                             , send :: Natural
                             , obliv :: Natural
                             }

data ProgramSize = ProgramSize { len :: Int
                               , algWidth :: Int
                               , secWidth :: Int
                               , flipWidth :: Int
                               , outWidth :: Int
                               , parties :: NonEmpty Party
                               , bodyRatios :: BodyRatios
                               , xorPreference :: Natural
                               , notFrequency :: Double
                               , sendEagerness :: Double
                               , oblivComplexity :: Double
                               }

generateProgram :: Gen (Program Identity)
generateProgram = do size <- getSize
                     let params = ProgramSize{ len = size
                                             , algWidth = squareRoot size
                                             , secWidth = 1 `max` (size `div` 10)
                                             , flipWidth = 1 `max` (size `div` 5)
                                             , outWidth = 1 `max` (size `div` 15)
                                             , parties = honest :| [corrupt]
                                             , bodyRatios = BodyRatios{compute=3, send=2, obliv=1}
                                             , xorPreference = 4
                                             , notFrequency = 0.2
                                             , sendEagerness = 0.5
                                             , oblivComplexity = 0.5
                                             }
                     q <- mkStdGen <$> arbitrary
                     return $ fst . run . runWriter . evalState mempty . runRandom q . runReader params $ buildProgram

buildProgram :: forall r.
                (ProgramBuilder r) =>
                Sem r ()
buildProgram = do buildSecrets
                  buildFlips
                  buildBody
                  buildOutputs

buildSecrets :: forall r.
                (ProgramBuilder r) =>
                Sem r ()
buildSecrets = do n <- asks secWidth >>= randomR . (0, )
                  sequence_ [ do p <- asks parties >>= oneOf
                                 tell $ Secret (iii $ Variable $ "sec" ++ show i) (iii p)
                              | i <- [1 .. n]
                            ]

buildFlips :: forall r.
              (ProgramBuilder r) =>
              Sem r ()
buildFlips = do n <- asks flipWidth >>= randomR . (0, )
                sequence_ [ do p <- asks parties >>= oneOf
                               tell $ Flip (iii $ Variable $ "flp" ++ show i) (iii p)
                            | i <- [1 .. n]
                          ]

buildBody :: forall r.
             (ProgramBuilder r) =>
             Sem r ()
buildBody = do n <- asks len
               BodyRatios{compute, send, obliv} <- asks bodyRatios
               when (n > 0) $ do
                 used <- join $ weighted $ (compute, buildCompute) :| [(send, buildSend), (obliv, buildOblivious)]
                 local (\size@ProgramSize{len} -> size{len = len - used}) buildBody

buildOutputs :: forall r.
                (ProgramBuilder r) =>
                Sem r ()
buildOutputs = do n <- asks outWidth >>= randomR . (1, )
                  boundVars <- gets $ keys . variables
                  selected <- n `manyOf` boundVars
                  sequence_ [ tell $ Output $ iii var
                              | var <- selected
                            ]

buildCompute :: forall r.
                (ProgramBuilder r) =>
                Sem r Int
buildCompute = do allVariables <- gets variables
                  allParties <- gets parties
                  participants <- oneOf $ top `cons` (singleton <$> allParties)
                  let vars = keys $ Map.filter (isJust . (participants `intersect`)) allVariables
                  let buildAlg :: Sem r (Algebra Identity)
                      buildAlg = do size <- asks algWidth
                                    alg <- if size < 1
                                             then join $ weighted $ (1, Literal . iii . Bit <$> random)
                                                                    :| [(10, Var . iii <$> chooseVar) | let Just chooseVar = oneOf <$> nonEmpty vars]
                                                                    -- TODO: bias to fresher vars?
                                             else do left <- randomR (1, size)
                                                     a <- local (\s -> s{algWidth = left}) buildAlg
                                                     b <- local (\s@ProgramSize{algWidth} -> s{algWidth = algWidth - left}) buildAlg
                                                     xp <- asks xorPreference
                                                     op <- weighted $ (1, And) :| [(xp, Xor)]
                                                     return $ iii a `op` iii b
                                    nf <- asks notFrequency
                                    case alg of
                                      Not _ -> return alg
                                      _ -> bool alg (Not $ iii alg) <$> bernoulli nf
                  w <- asks algWidth >>= randomR . (1, ) . min (length vars)  -- Should we be truncating like this?
                  alg <- local (\s -> s{algWidth = w}) buildAlg
                  tell $ Compute (iii . Variable $ "v" ++ show (length allVariables)) (iii alg)
                  return 1

buildSend :: forall r.
             (ProgramBuilder r) =>
             Sem r Int
buildSend = do boundVars <- gets $ Map.toList . variables
               allParties <- toList <$> asks parties
               let options = [ (possibleRecipients, var)
                               | (var, owners) <- boundVars
                               , let possibleRecipients = [p | p <- allParties , not (p `isElementOf` owners)]
                               , not (null possibleRecipients)
                             ]
               case options of [] -> return 0
                               v : vs -> do (possibleRecipients, var) <- oneOf $ v :| vs
                                            se <- (1 -) <$> asks sendEagerness
                                            ps <- attrit se possibleRecipients -- if none of them make it, it goes to everyone!
                                            tell $ Send (iii $ Parties $ S.fromList ps)  (iii var)
                                            return 1

data OblivOption = OblivOption { recipients :: PartySet
                               , sendable :: [Variable]
                               , choosable :: [Variable]
                               }

buildOblivious :: forall r.
                  (ProgramBuilder r) =>
                  Sem r Int
buildOblivious = do allParties <- asks $ S.fromList . toList . parties
                    let subsets = S.toList . S.delete allParties . S.delete mempty . S.powerSet $ allParties
                    boundVars <- gets $ Map.toList . variables
                    se <- (1 -) <$> asks sendEagerness
                    options <- sequence [ do let complement = allParties `S.difference` senders
                                             rHead <- oneOf . fromList . S.toList $ complement  -- it should go to someone _new_.
                                             rTail <- attrit se $ S.toList allParties
                                             let recipients = Parties . S.fromList $ rHead : rTail
                                             return OblivOption{ recipients
                                                               , sendable = [v | (v, owners) <- boundVars , Parties senders `isSubsetOf` owners]
                                                               , choosable = [v | (v, owners) <- boundVars , recipients `isSubsetOf` owners] }
                                          | senders <- subsets
                                        ]
                    let weight oo@OblivOption{sendable, choosable} = let w = fromIntegral $ length sendable `min` 2 ^ length choosable
                                                                     in if 2 <= w then Just (w, oo) else Nothing
                    let makeBody = do recurseLikelyhood <- asks oblivComplexity
                                      undefined
                    case mapMaybe weight options of [] -> return 0
                                                    o : os -> do opt <- weighted $ o :| os
                                                                 subjects <- shuffle $ sendable opt
                                                                 choices <- shuffle $ choosable opt
                                                                 body <- makeBody   
                                                                 tell $ Oblivious (iii . Variable $ "obliv" ++ show (length boundVars))
                                                                                  (iii $ recipients opt) (f (ObvBody f))
                                                                 return 1

tell :: forall r.
        (ProgramBuilder r) =>
        Statement Identity -> Sem r ()
tell stmnt = do case stmnt of
                  Secret fv fp -> bindToParties (jjj fv) (singleton $ jjj fp)
                  Flip fv fp -> bindToParties (jjj fv) (singleton $ jjj fp)
                  Compute fv falg -> do (ps, _) <- failOnError . validateAlg . antimap' ((Pos.newPos "__INVISIBLE" 0 0,) . runIdentity) $ falg
                                        bindToParties (jjj fv) ps
                  Send fps fv -> bindToParties (jjj fv) (jjj fps)
                  Oblivious fv fps _ -> bindToParties (jjj fv) (jjj fps)
                  Output _ -> return ()
                  Declaration _fn _pargs _body -> undefined
                  Call _fn _pargs _bindings -> undefined
                W.tell [iii stmnt]

