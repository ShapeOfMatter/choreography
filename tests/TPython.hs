module TPython where

import Distribution.TestSuite.QuickCheck
import Test.QuickCheck (ioProperty)

import Python
import Utils (throwsException)

tests :: IO [Test]
tests = do return [ testHealth
                  , antiHealth
                  , exampleFromFile]


testHealth :: Test
testHealth = testProperty "Health Check" $ ioProperty $ healthCheck [PythonLibrary "numpy", PythonLibrary "sklearn"]

antiHealth :: Test
antiHealth = testProperty "Healthcheck could actually fail"
  $ ioProperty
  $ throwsException @IOError
  $ healthCheck [PythonLibrary "asdfasfasdfasdfjjj"]

exampleFromFile :: Test
exampleFromFile = testProperty "Run examples/dt_test.py" $ ioProperty do
  code <- readFile "examples/dt_test.py"
  (idealScore, realScore) <- runPythonCommand @(Double, Double) code
  return $ abs (0.5 - idealScore) < 0.15 && abs (0.5 - realScore) < 0.15
