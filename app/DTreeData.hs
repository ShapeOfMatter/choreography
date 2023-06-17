module DTreeData where

import Data.List (uncons)
import qualified Data.Set as Set
import Text.Parsec.String (parseFromFile)
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Choreography
import Utils

main :: IO ()
main = do
  args <- getArgs
  case readMaybe @Int <$$$> uncons args of
    Just (programFile, [Just iterations, Just trainingN, Just testingN]) -> do
      program' <- parseFromFile (changeState (const ()) (const mempty) programParser) programFile
      let program = either error id $ either (error . show) id $ validate mempty <$> program'
      let corruption = Parties $ Set.fromList [corrupt, p1]
      printParallelized iterations trainingN testingN program corruption
    _ -> do putStrLn "Useage: d-tree-data file.cho iterations trainingN testingN"
