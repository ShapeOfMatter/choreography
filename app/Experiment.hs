module Experiment where

import Control.Exception (catch, SomeException)
import Control.Monad (forM)
import qualified Data.ByteString.Lazy as ByteString
import Data.Csv (encode)
import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Word (Word64)
import GHC.Exts (fromList)
import Numeric.Natural (Natural)
import Options.Applicative ( (<**>)
                           , auto
                           , execParserPure
                           , handleParseResult
                           , help
                           , helper
                           , info
                           , long
                           , metavar
                           , option
                           , Parser
                           , prefs
                           , progDesc
                           , short
                           , strOption
                           , value
                           )
import System.Environment (getArgs)
import System.Random (newStdGen)
import Text.Read (readMaybe)

import Choreography hiding (value, writeCSV)
import Utils ((<$$$>))

type Logger = String -> IO ()

data Settings = Settings { sizing :: ProgramSize
                         , iters :: [IterConfig]
                         , alpha :: Double
                         } deriving (Read, Show)

data Arguments = Arguments { settingsFile :: FilePath
                           , destination :: FilePath
                           , filePrefix :: String
                           , save :: Natural
                           , totalGenerated :: Natural
                           }

argParser :: Parser Arguments
argParser = do
    settingsFile <- strOption (     long "settings"       <> short 's' <> metavar "PATH"
                                    <> help "A .settings file from which to `read` the `Settings` object.")
    destination <- strOption (      long "destination"       <> short 'd' <> metavar "PATH"
                                    <> help "Directory in which to write passing protocols.")
    filePrefix <- strOption (       long "prefix"            <> short 'p' <> metavar "WORD" <> value ""
                                    <> help "Prefix for sequential filenames, to distinguish between runs.")
    save <- option auto (           long "save-n"          <> short 'w' <> metanat <> value (-1 :: Int)
                                    <> help "How many examples to save to disk. (Negatives wrap from n+1.)")
    totalGenerated <- option auto ( long "generate"          <> short 'n' <> metanat
                                    <> help "The total number of programs to generate and test.")
    return Arguments{settingsFile, destination,
                     filePrefix = case filePrefix of [] -> ""; fp -> fp ++ "_",
                     save = if save < 0 then totalGenerated + 1 - fromIntegral save else fromIntegral save,
                     totalGenerated}
  where metanat = metavar "NAT"

main :: IO ()
main = do args <- getArgs
          Arguments{settingsFile, destination, filePrefix, save, totalGenerated} <- handleParseResult $
            execParserPure (prefs mempty)
                           (info (argParser <**> helper)
                                 (progDesc $ "Randomly generate .cho protocols and check their security"
                                           ++ " using the provided dtree settings.")) args
          Just settings@Settings{iters, alpha} <- readMaybe <$> readFile settingsFile
          time <- getCurrentTime
          let runName = filePrefix ++ takeWhile (/= '.') settingsFile ++ formatTime defaultTimeLocale "%04Y_%b_%d_%H_%M_%S_" time
          let fileNames = [(i <= save, destination ++ "/" ++ runName ++ show i ++ ".cho")
                           | i :: Natural <- [1..totalGenerated]]
          let writeLog = appendFile $ destination ++ "/" ++ runName ++ "log.txt"
          results <- catMaybes <$> mapM (blindDetermination writeLog . uncurry (attempt writeLog settings)) fileNames
          writeSankey (destination ++ "/" ++ runName ++ ".sankey.txt")
            . sankey alpha
            . zip (testName <$> iters)
            . transpose
            . (snd <$>)
            $ results
          writeCSV (destination ++ "/" ++ runName ++ ".csv") alpha (testName <$> iters) results

attempt :: Logger -> Settings -> Bool -> FilePath -> IO (String, [Double])
attempt writeLog Settings{sizing, iters} write destination = do
  q <- newStdGen
  let cho = either error id . validate mempty . snd . fakePos 0 $ randomProgram sizing q
  writeLog $ "Generated " ++ show (length cho) ++ " line program. "
  pvals <- forM iters \iter -> do pval <- experiment_ @Word64 iter cho corruption
                                  writeLog $ "Measured p-value: " ++ show pval ++ " "
                                  return pval
  if write
    then do writeFile destination $ unlines [makeHeader sizing (iters `zip` pvals),
                                             render cho]
            writeLog $ "Wrote out to \"" ++ destination ++ "\"."
    else writeLog "Discarding. "
  return (destination, pvals)

blindDetermination :: Logger -> IO a -> IO (Maybe a)
blindDetermination writeLog task = do retval <- catch (Just <$> task)
                                                      \(e :: SomeException) -> do
                                                          let m = show e
                                                          writeLog if "ValueError: Found array with 0 feature(s)" `isInfixOf` m
                                                                     then "Ignoring un-assessable protocol."
                                                                     else m
                                                          return Nothing
                                      writeLog "\n"
                                      return retval

corruption :: PartySet
corruption = Parties $ fromList [corrupt, p1]

makeHeader :: ProgramSize -> [(IterConfig, Double)] -> String
makeHeader sizing tests = unlines [ "{-"
                                       , show sizing
                                       , show tests
                                       , "-}"
                                       ]

testName :: IterConfig -> String
testName IterConfig{iterations, trainingN, testingN} = "test_" ++ show iterations ++ "_" ++ show trainingN ++ "_" ++ show testingN


sankey :: Double -> [(String, [Double])] -> [(String, Int, String)]
sankey alpha pvals = let thresholded = (<= alpha) <$$$> pvals
                         transitions = [ (name1, name2, [\(b1, b2) -> (b1 == v1) && (b2 == v2) | (v1, v2) <- vals1 `zip` vals2])
                                         | ((name1, vals1), (name2, vals2)) <- thresholded `zip` tail thresholded ]
                         failName = ("F_" ++)
                         generations = [("Generated"
                                        ,length . filter (== pass) . snd . head $ thresholded
                                        ,(if pass then fst . head else failName . fst . head) thresholded)
                                        | pass <- [True, False]]
                     in generations ++ do (name1, name2, ts) <- transitions
                                          pass1 <- [True, False]
                                          pass2 <- [True, False]
                                          return (if pass1 then name1 else failName name1
                                                 ,length $ filter ($ (pass1, pass2)) ts
                                                 ,if pass2 then name2 else failName name2)

writeSankey :: FilePath -> [(String, Int, String)] -> IO ()
writeSankey f = writeFile f . unlines . (format <$>)
  where format (name1, quantity, name2) = name1 ++ "[" ++ show quantity ++ "]" ++ name2

writeCSV :: FilePath -> Double -> [String] -> [(String, [Double])] -> IO ()
writeCSV f alpha testNames results = ByteString.writeFile f . encode $ header : body
  where header = "filename" : concat [ [name ++ "_p", name ++ "_secure"]
                                       | name <- testNames]
        body = [ filename : concat [ [show p, show (p <= alpha)]
                                     | p <- ps]
                 | (filename, ps) <- results]
