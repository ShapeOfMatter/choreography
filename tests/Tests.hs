module Tests where

import Data.Map.Strict (fromList, singleton)
import Distribution.TestSuite.QuickCheck
import Test.QuickCheck ((===))

import Choreography.AbstractSyntaxTree
import Choreography.CoreSemantics
import Choreography.Party


tests :: IO [Test]
tests = return [tautology, example]


tautology :: Test
tautology = testProperty "Tautology" \i -> (===) @Int i i

example :: Test
example = testProperty "An easy example" $ Right (outputs, views) === deterministicEvaluation program inputs tapes
  where program = [(0, Variable p1 "rand", Flip p1)
                  ,(1, Variable p2 "sec", Secret p2)
                  ,(2, Variable p2 "comp1", Compute $ Not $ Xor (Var $ Variable p2 "sec") (Literal p2 True))
                  ,(3, Variable p1 "m1", Send p1 $ Var $ Variable p2 "comp1")
                  ,(4, Variable p1 "y", Output $ And (Var $ Variable p1 "rand") (Var $ Variable p1 "m1"))]
        inputs = Inputs $ singleton (Variable p2 "sec") True
        tapes = Tapes $ singleton (Variable p1 "rand") True
        views = Views $ fromList [(Variable p1 "m1", True), (Variable p1 "rand", True)]
        outputs = Outputs $ singleton (Variable p1 "y") True
