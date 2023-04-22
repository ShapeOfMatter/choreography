module TPython where

import Data.Map.Strict (fromList, singleton, toList)
import Distribution.TestSuite.QuickCheck
import Test.QuickCheck (arbitrary, Gen, ioProperty, Property, vectorOf, counterexample)
import Text.Parsec.String (parseFromFile)

import Choreography
import Python
import Utils (changeState, throwsException)

tests :: IO [Test]
tests = do return [ testHealth
                  , antiHealth
                  , exampleFromFile
                  , gmwAndGates]


testHealth :: Test
testHealth = testProperty "Health Check" $ ioProperty $ healthCheck [PythonLibrary "numpy", PythonLibrary "sklearn"]

antiHealth :: Test
antiHealth = testProperty "Healthcheck could actually fail"
  $ ioProperty
  $ throwsException @IOError
  $ healthCheck [PythonLibrary "asdfasfasdfasdfjjj"]

exampleFromFile :: Test
exampleFromFile = testProperty "Run examples/dt_test.py" $ ioProperty do
  code <- pythonLines <$> readFile "examples/dt_test.py"
  (idealScore, realScore) <- runPythonCommand @(Double, Double) code
  return $ abs (0.5 - idealScore) < 0.15 && abs (0.5 - realScore) < 0.15

gmwAndGates :: Test
gmwAndGates = testProperty "Two party three-arg AND in GMW (python)" $ ioProperty gmwAndGatesIO
gmwAndGatesIO :: IO (Gen Property)
gmwAndGatesIO = do
  program' <- parseFromFile (changeState (const ()) (const mempty) programParser) "examples/3party2andGMW.cho"
  let program = either (error . show) id program'
  return do  -- The Gen Monad!
    secrets <- vectorOf 3 (arbitrary @Bool)
    let inputs = Inputs $ fromList $ [Variable "c_in"
                                     ,Variable "h1_in"
                                     ,Variable "h2_in"] `zip` secrets
    randomness <- vectorOf 5 (arbitrary @Bool)
    let tapes = Tapes $ fromList $ [Variable "c_s1"
                                   ,Variable "h1_s1"
                                   ,Variable "h2_s1"
                                   ,Variable "g1_s1"
                                   ,Variable "g2_s2"] `zip` randomness
    let (outputs, views) = deterministicEvaluation program inputs tapes
    return $ ioProperty do -- Back in the IO monad!?
      (os, vs, code) <- runPythonProgram program inputs tapes
      let messages = [ asString code,
                      "inputs = " ++ show (toList $ inputsMap inputs),
                      "tapes = " ++ show (toList $ tapesMap tapes),
                      "views = " ++ show (concat $ toList <$> viewsMap vs),
                      "outputs = " ++ show (concat $ toList <$> outputsMap os),
                      "expected views = " ++ show (concat $ toList <$> outputsMap outputs)]
      return $ counterexample (unlines messages) $ os == outputs && vs == views
