module Tests where

import Data.Map.Strict (fromList, singleton)
import Distribution.TestSuite.QuickCheck
import Polysemy (run)
import Polysemy.Error (runError)
import Test.QuickCheck ((===), Arbitrary (arbitrary), vectorOf)
import Text.Parsec.String (parseFromFile)

import Choreography.AbstractSyntaxTree
import Choreography.CoreSemantics
import Choreography.Parser (programParser)
import Choreography.Party
import Choreography.EasySyntaxTree (compileSem)


tests :: IO [Test]
tests = do piplineTest <- pipeline
           return [tautology, example, piplineTest]


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

pipeline :: IO Test
pipeline = do Right pass1 <- parseFromFile programParser "examples/3partyXOR.cho"
              let Right pass2 = run . runError $ compileSem pass1
              return $ testProperty "3partyXOR.cho gives correct outputs" do  -- The Gen Monad!
                let (c, h1, h2) = (Party "c", Party "h1", Party "h2")
                secrets <- vectorOf 3 (arbitrary @Bool)
                let inputs = Inputs $ fromList $ [Variable c "c_in"
                                                 ,Variable h1 "h1_in"
                                                 ,Variable h2 "h2_in"] `zip` secrets
                randomness <- vectorOf 6 (arbitrary @Bool)
                let tapes = Tapes $ fromList $ [Variable c "c_s1"
                                               ,Variable c "c_s2"
                                               ,Variable h1 "h1_s1"
                                               ,Variable h1 "h1_s2"
                                               ,Variable h2 "h2_s1"
                                               ,Variable h2 "h2_s2"] `zip` randomness
                let outputs = Outputs $ fromList $ [Variable c "y_c"
                                                   ,Variable h1 "y_h1"
                                                   ,Variable h2 "y_h2"] `zip` repeat (foldl (/=) False secrets)
                let Right (observed, _) = deterministicEvaluation pass2 inputs tapes
                return $ observed === outputs
