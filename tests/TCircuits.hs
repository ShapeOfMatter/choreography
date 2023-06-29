module TCircuits where

import Data.Maybe (isJust, isNothing)
import Distribution.TestSuite.QuickCheck
import Test.QuickCheck (conjoin, counterexample)

import Circuit
import Utils (pretty)

tests :: IO [Test]
tests = do return [ soundAndComplete
                  , correctAdder]


soundAndComplete :: Test
soundAndComplete = testProperty "All and only valid circuits evaluate." $ \circuit ->
  let outputs = basicEvaluations [] circuit
      validity = validations [] circuit
  in counterexample (show (pretty circuit, validity, outputs)) if validity then isJust `all` outputs else isNothing `any` outputs

correctAdder :: Test
correctAdder = testProperty "The adder circuits actually work." $ \xs ys ->
  let width = length xs `max` length ys
      x = sum [fromEnum b * (2 ^ i) | (b, i :: Int) <- xs `zip` [0..]]
      y = sum [fromEnum b * (2 ^ i) | (b, i :: Int) <- ys `zip` [0..]]
      ans = x + y
      xNames = ["x" ++ show i | i <- [0 .. width - 1]]
      yNames = ["y" ++ show i | i <- [0 .. width - 1]]
      circuit = adder $ xNames `zip` yNames
      arguments = (xNames `zip` (xs ++ repeat False)) ++ (yNames `zip` (ys ++ repeat False))
      results = basicEvaluations arguments circuit
  in conjoin [ counterexample (show (xs, ys, pretty circuit)) $ validations (xNames ++ yNames) circuit
             , counterexample (show (xs, ys, pretty circuit, results)) $ isJust `all` results
             , counterexample (show (xs, ys, pretty circuit, results, ans)) $ ans == sum [fromEnum b * (2 ^ i) | (Just b, i :: Int) <- results `zip` [0..]]
             ]
