module Choreography.DecisionTree where

import Control.Monad (replicateM)
import Data.ByteString.Lazy (hPut)
import Data.Csv (encodeByName, Header, header, toField)
import Data.Functor.Identity (Identity(Identity))
import Data.Int (Int8)
import Data.Map ((!), fromList, Map, toList)
import Data.Tuple.Select (sel1, sel2, sel3)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.IO.Handle (Handle)
import GHC.IO.Handle.FD (stdout)
import Polysemy (Members, runM, Sem)
import Polysemy.Random (Random, random, runRandomIO)

import Choreography.AbstractSyntaxTree (Proper, Program, Variable (Variable))
import Choreography.Metadata (metadata, ProgramMetaData(..))
import Choreography.Party (isElementOf, PartySet)
import Choreography.Semantics (Inputs(..), Outputs(..), Views(..), deterministicEvaluation)
import Python ()
import Utils ((<$$>), Pretty1)

type FieldName = String
type Extraction = (Inputs, Outputs, Views) -> Bool

asByte :: Bool -> Int8
asByte True = 1
asByte False = 0

-- Returns a 2-d array; all interal structure is implicit.
makeData :: forall f r.
            (Members '[Random] r,
             Proper f, Functor f, Traversable f, Pretty1 f) =>
            Int -> Int -> Int -> Program f -> [(Extraction, FieldName)] -> Sem r [Map FieldName Int8]
makeData iterations trainingN testingN p h = V.toList <$> flatten <$$> vectorSemantics (iterations * (trainingN + testingN)) p
    where flatten iov = fromList [(name, asByte $ f iov) | (f, name) <- h]

structureFields :: (Functor f, Pretty1 f, Proper f) =>
                   Program f -> PartySet -> [(Extraction, FieldName)]
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

vectorSemantics :: forall f r.
                   (Members '[Random] r,
                    Proper f, Functor f, Traversable f, Pretty1 f) =>
                   Int -> Program f -> Sem r (Vector (Inputs, Outputs, Views))
vectorSemantics n p = V.replicateM n trial
  where ProgramMetaData {inputVars, tapeVars} = metadata p
        trial :: Sem r (Inputs, Outputs, Views)
        trial = do inputs <- Inputs . fromList <$> sequence [ (var,) <$> random | (_, var) <- inputVars]
                   tapes <- replicateM (length tapeVars) random
                   let (os, vs) = deterministicEvaluation p inputs tapes
                   return (inputs, os, vs)

writeCSV :: Handle -> Header -> [Map FieldName Int8] -> IO ()
writeCSV file h records = do let bs = encodeByName h records
                             hPut file bs

printParallelized :: (Proper f, Functor f, Traversable f, Pretty1 f) =>
                     Int -> Int -> Int -> Program f -> PartySet -> IO ()
printParallelized iterations trainingN testingN p corruption = do let fields = structureFields p corruption
                                                                  let h = asHeader fields
                                                                  body <- runM . runRandomIO $ makeData iterations trainingN testingN p fields
                                                                  writeCSV stdout h body
