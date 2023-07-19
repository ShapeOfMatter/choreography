module Choreography.DecisionTree where

import Control.Monad (replicateM, when)
import Data.Bits (FiniteBits, finiteBitSize, testBit)
import Data.ByteString.Lazy (hPut)
import Data.Csv (encodeByName, Header, header, toField)
import Data.Functor.Identity (Identity(Identity))
import Data.Int (Int8)
import Data.Map ((!), fromList, Map, toList)
import Data.Tuple.Select (sel1, sel2, sel3)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.IO.Handle (Handle, hPutStr)
import GHC.IO.Handle.FD (stderr, stdout)
import Polysemy (Members, runM, Sem)
import Polysemy.Random (Random, random, runRandomIO)
import qualified System.Random as R

import Choreography.AbstractSyntaxTree (Program, Variable (Variable))
import Choreography.Functors (Proper)
import Choreography.Metadata (metadata, ProgramMetaData(..))
import Choreography.Party (isElementOf, PartySet)
import Choreography.Semantics (Inputs(..), Outputs(..), Views(..), deterministicEvaluation, Semanticable)
import Python ()
import Utils ((<$$>), Pretty1)

type FieldName = String
type Extraction w = (Inputs w, Outputs w, Views w) -> w

asByte :: Bool -> Int8
asByte True = 1
asByte False = 0

requestedIterations :: (Num a) => a -> a -> a -> a
requestedIterations iterations trainingN testingN = iterations * (trainingN + testingN)

batchesOf :: forall w a. (FiniteBits w, Integral a) => a -> (a, a)
batchesOf iterations = let batchSize = fromIntegral $ finiteBitSize (undefined :: w)
                           (basic, extra) = iterations `divMod` batchSize
                           batches = basic + signum extra
                       in (batches, batchSize)

-- Returns a 2-d array; all interal structure is implicit.
makeData :: forall w f r.
            (Members '[Random] r,
             Proper f, Functor f, Traversable f, Pretty1 f,
             Semanticable w, R.Uniform w) =>
            Int -> Program f -> [(Extraction w, FieldName)] -> Sem r [Map FieldName w]
makeData batches p h = V.toList <$> flatten <$$> vectorSemantics batches p
    where flatten iov = fromList [(name, f iov) | (f, name) <- h]

structureFields :: (Functor f, Pretty1 f, Proper f) =>
                   Program f -> PartySet -> [(Extraction w, FieldName)]
structureFields p corruption = ideal <> view <> honest
  where ProgramMetaData { inputVars -- :: [(Concrete Party, Variable)]
                        , viewVars -- :: Map (Concrete Party) [Variable]
                        , outputVars -- :: Map (Concrete Party) [Variable]
                        } = metadata p
        ideal = corruptInputs <> corruptOutputs
        corruptInputs = [((! var) . inputsMap . sel1, buildName "i_" var Nothing)
                         | (Identity party, var) <- inputVars
                         , party `isElementOf` corruption]
        corruptOutputs = concat [[((! var) . (! Identity party) . outputsMap . sel2, buildName "i_" var Nothing) | var <- vars]
                                 | (Identity party, vars) <- toList outputVars
                                 , party `isElementOf` corruption]
        view = concat [concat [ [((!! (n - 1)) . (! var) . (! Identity party) . viewsMap . sel3, buildName "v_" var (Just n))
                                 | n <- [1 .. count]]
                                | (var, count) <- toList vars ]
                       | (Identity party, vars) <- toList viewVars
                       , party `isElementOf` corruption]
        honest = [((! var) . inputsMap . sel1, buildName "h_" var Nothing)
                         | (Identity party, var) <- inputVars
                         , not $ party `isElementOf` corruption]
        buildName :: String -> Variable -> Maybe Int -> FieldName
        buildName prefix (Variable var) Nothing = prefix <> var
        buildName prefix (Variable var) (Just n) = prefix <> var <> "_" <> show n

asHeader :: [(a, FieldName)] -> Header
asHeader = header . (toField . snd <$>)

vectorSemantics :: forall w f r.
                   (Members '[Random] r,
                    Proper f, Functor f, Traversable f, Pretty1 f,
                    Semanticable w, R.Uniform w) =>
                   Int -> Program f -> Sem r (Vector (Inputs w, Outputs w, Views w))
vectorSemantics n p = V.replicateM n trial
  where ProgramMetaData {inputVars, tapeVars} = metadata p
        trial :: Sem r (Inputs w, Outputs w, Views w)
        trial = do inputs <- Inputs . fromList <$> sequence [ (var,) <$> random | (_, var) <- inputVars]
                   tapes <- replicateM (length tapeVars) random
                   let (os, vs) = deterministicEvaluation p inputs tapes
                   return (inputs, os, vs)

expand :: forall w f. (Functor f, FiniteBits w) => f w -> [f Int8]
expand fw = [ asByte . (`testBit` i) <$> fw | i <- [0 .. finiteBitSize (undefined :: w) - 1] ]

writeCSV :: (FiniteBits w) => Handle -> Header -> [Map FieldName w] -> IO ()
writeCSV file h records = do let bs = encodeByName h $ concat (expand <$> records)
                             hPut file bs

printParallelized :: forall w f.
                     (Proper f, Functor f, Traversable f, Pretty1 f,
                      Semanticable w, R.Uniform w) =>
                     Int -> Int -> Int -> Program f -> PartySet -> IO ()
printParallelized iterations trainingN testingN p corruption = do
    let fields = structureFields p corruption
    let h = asHeader fields
    let n = requestedIterations iterations trainingN testingN
    let (batches, batchSize) = batchesOf @w n
    let n' = batches * batchSize
    when (n /= n')
        (hPutStr stderr $ "Requested " ++ show n ++ " lines of data; generating " ++ show n' ++ ".\n")
    body <- runM . runRandomIO $ makeData batches p fields
    writeCSV @w stdout h body
