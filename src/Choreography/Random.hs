module Choreography.Random where

import Control.Monad (when, join)
import Data.Bool (bool)
import Data.Functor.Identity (Identity(Identity), runIdentity)
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import Data.Map (keys)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import GHC.Exts (fromList, IsList, Item, toList)
import Numeric.Natural (Natural)
import Polysemy (Members, run, Sem)
import Polysemy.Random (attrit, manyOf, oneOf, Random, random, randomR, runRandom, weighted, bernoulli, shuffle)
import Polysemy.Reader (asks, local, Reader, runReader)
import Polysemy.State (evalState, gets, State)
import Polysemy.Writer (runWriter, Writer)
import qualified Polysemy.Writer as W -- so we don't use the wrong tell by accident.
import Test.QuickCheck (Gen, getSize, Arbitrary (arbitrary))
import qualified Text.Parsec.Pos as Pos
import System.Random (mkStdGen, RandomGen)

import Choreography.AbstractSyntaxTree
import Choreography.Functors (antimap')
import Choreography.Party (corrupt, honest, isElementOf, isSubsetOf, Party, singleton, PartySet (Parties))
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
                             } deriving (Show)

data ProgramSize = ProgramSize { len :: Int
                               , algWidth :: Int
                               , secWidth :: Int
                               , flipWidth :: Int
                               , outWidth :: Int
                               , participants :: NonEmpty Party
                               , bodyRatios :: BodyRatios
                               , xorPreference :: Natural
                               , notFrequency :: Double
                               , sendEagerness :: Double
                               , oblivComplexity :: Double
                               } deriving (Show)

randomProgram :: (RandomGen q) => ProgramSize -> q -> Program Identity
randomProgram params q = fst . run . runWriter . evalState mempty . runRandom q . runReader params $ buildProgram

generateProgram :: Gen (Program Identity)
generateProgram = do size <- getSize
                     let params = ProgramSize{ len = size
                                             , algWidth = squareRoot size
                                             , secWidth = 1 `max` (size `div` 10)
                                             , flipWidth = 1 `max` (size `div` 5)
                                             , outWidth = 1 `max` (size `div` 15)
                                             , participants = honest :| [corrupt]
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
                  sequence_ [ do p <- asks participants >>= oneOf
                                 tell $ Secret (iii $ Variable $ "sec" ++ show i) (iii p)
                              | i <- [1 .. n]
                            ]

buildFlips :: forall r.
              (ProgramBuilder r) =>
              Sem r ()
buildFlips = do n <- asks flipWidth >>= randomR . (0, )
                sequence_ [ do p <- asks participants >>= oneOf
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
                  allVars <- gets $ toList . variables
                  let useableVars = [ var
                                      | (var, Parties ps) <- allVars
                                      , not (null ps)  -- we filter out TOP, bc that's not implemented. This _should only affect literals.
                                      ]
                  selected <- n `manyOf` useableVars
                  sequence_ [ tell $ Output $ iii var
                              | var <- selected
                            ]

buildCompute :: forall r.
                (ProgramBuilder r) =>
                Sem r Int
buildCompute = do allVariables <- gets variables
                  allParties <- asks participants
                  -- Because allParties is nonEmpty, the powerset will be nonempty even after we remove the empty set.
                  -- This could be done other ways, but this has the advantage that it's not biased toward `top` computations.
                  ps <- oneOf . (Parties <$>) . fromList . toList . S.delete mempty . S.powerSet . fromList . toList $ allParties
                  let vars = keys $ Map.filter (ps `isSubsetOf`) allVariables
                  let buildAlg :: Sem r (Algebra Identity)
                      buildAlg = do size <- asks algWidth
                                    alg <- if size <= 1
                                             then join $ weighted $ (1, Literal . iii . Bit <$> random)
                                                                    :| maybe [] ((:[]) . (10,) . (Var . iii <$>)) (oneOf <$> nonEmpty vars)
                                                                    -- TODO: bias to fresher vars?
                                             else do left <- randomR (1, size - 1)
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
buildSend = do boundVars <- gets $ toList . variables
               allParties <- toList <$> asks participants
               let options = [ (fromList possibleRecipients, var)
                               | (var, owners) <- boundVars
                               , let possibleRecipients = [p | p <- allParties , not (p `isElementOf` owners)]
                               , not (null possibleRecipients)
                             ]
               case options of [] -> return 0
                               v : vs -> do (possibleRecipients, var) <- oneOf $ v :| vs
                                            se <- (1 -) <$> asks sendEagerness
                                            rHead <- oneOf possibleRecipients -- it has to go to at least one new person.
                                            rTail <- attrit se allParties
                                            tell $ Send (iii . Parties . fromList $ rHead : rTail)  (iii var)
                                            return 1

data OblivOption f1 f2 = OblivOption { recipients :: PartySet
                                     , sendable :: f1 Variable
                                     , choosable :: f2 Variable
                                     }

buildOblivious :: forall r.
                  (ProgramBuilder r) =>
                  Sem r Int
buildOblivious = do allParties <- asks $ fromList . toList . participants
                    let subsets = toList . S.delete allParties . S.delete mempty . S.powerSet $ allParties
                    boundVars <- gets $ toList . variables
                    se <- (1 -) <$> asks sendEagerness
                    options <- sequence [ do let complement = allParties `S.difference` senders
                                             -- NonEmpty.fromList should be safe here bc the S.delete(s) above ensure a non-empty complement.
                                             rHead <- oneOf . fromList . toList $ complement  -- it should go to someone _new_. (necessary?)
                                             rTail <- attrit se $ toList allParties  -- TODO: this construction is bias toward medium-size recipients lists.
                                             -- ideally we would be biased toward recipient lists that have variables we want to use.
                                             let recipients = Parties . fromList $ rHead : rTail
                                             return OblivOption{ recipients
                                                               , sendable = [v | (v, owners) <- boundVars , Parties senders `isSubsetOf` owners]
                                                               , choosable = [v | (v, owners) <- boundVars , recipients `isSubsetOf` owners] }
                                          | senders <- subsets
                                        ]
                    case mapMaybe weight options of [] -> return 0
                                                    o : os -> do opt <- weighted $ o :| os
                                                                 subjects <- shuffle $ sendable opt
                                                                 choices <- shuffle $ choosable opt
                                                                 body <- makeBody subjects choices
                                                                 tell $ Oblivious (iii . Variable $ "obliv" ++ show (length boundVars))
                                                                                  (iii $ recipients opt) (iii body)
                                                                 return 1
  where weight OblivOption{recipients, sendable, choosable} = case (sendable, choosable) of
                                                                (s1 : s2 : ss, c : cs) -> Just ( min (fromIntegral $ length sendable)
                                                                                                     (2 ^ length choosable)
                                                                                               , OblivOption{ recipients
                                                                                                            , sendable = (s1, s2) :|| ss
                                                                                                            , choosable = c :| cs } )
                                                                _ -> Nothing
        makeBody ((s1, s2) :|| subjects) (c :| choices) = do splitPoint <- randomR (0, length subjects)  -- we already shuffled.
                                                             let (ss1, ss2) = splitAt splitPoint subjects
                                                             rounding <- oneOf $ floor :| [ceiling]
                                                             let cPoint = rounding $ logBase @Double 2 $ fromIntegral splitPoint
                                                             let (cs1, cs2) = splitAt cPoint choices
                                                             branch1 <- makeBranch (s1 :| ss1) cs1
                                                             branch2 <- makeBranch (s2 :| ss2) cs2
                                                             return $ ObvBody (iii branch1) (iii branch2) (iii c)
        makeBranch    (s1 :| [])              _             = return $ ObvLeaf s1
        makeBranch    (s1 :| _)               []            = return $ ObvLeaf s1
        makeBranch ss@(s1 :| (s2 : subjects)) (c : choices) = do recurse <- asks oblivComplexity >>= bernoulli
                                                                 if recurse then ObvBranch <$> makeBody ((s1, s2) :|| subjects) (c :| choices)
                                                                            else ObvLeaf <$> oneOf ss

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
                  Declaration _fn _pargs _body -> undefined  -- TODO
                  Call _fn _pargs _bindings -> undefined
                W.tell [iii stmnt]

data DoubleNonEmpty a = (a, a) :|| [a] deriving (Eq, Functor, Ord, Show)
instance IsList (DoubleNonEmpty a) where
  type Item (DoubleNonEmpty a) = a
  fromList (x1 : x2 : xs) = (x1, x2) :|| xs
  fromList xs = error $ "Tried to make a DoubleNonEmpty from a list of length " ++ show (length xs) ++ "."
  toList ((x1, x2) :|| xs) = x1 : x2 : xs

