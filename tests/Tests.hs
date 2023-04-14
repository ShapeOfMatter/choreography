module Tests where

import Data.Map.Strict (fromList, singleton, empty)
import qualified Data.Set as Set
import Distribution.TestSuite.QuickCheck
import Test.QuickCheck ((===), Arbitrary (arbitrary), chooseInt, counterexample, Gen, ioProperty, Property, vectorOf)
import Text.Parsec (runParser)
import Text.Parsec.String (parseFromFile)

import Choreography.AbstractSyntaxTree
import Choreography.Parser hiding (owners)
import Choreography.Party
--import Choreography.EasySyntaxTree (compileSem)
import Choreography.Semantics
import Utils
import Data.List (nub)


tests :: IO [Test]
tests = do return [ tautology
                  , testEasyCompute
                  , testEasySecret
                  , testEasyFlip
                  , testEasySend
                  , testEasyObliv
                  , testLines
                  , testOutput
                  , testSend
                  , testObliv
                  , example
                  , pipeline
                  , oneOfFourOT]


tautology :: Test
tautology = testProperty "Tautology" \i -> (===) @Int i i

testEasyCompute :: Test
testEasyCompute = testProperty "Easy Compute" $ case compute of
    Compute (Location os' _, Variable "a") (_, Literal (_, Bit True)) | os' == os && os == top -> True
    _ -> error (pretty compute)
  where compute :: Statement Located
        Right (Location os _, compute) = runParser expressionParser empty "hardcoded example" "a = 1"

testEasySecret :: Test
testEasySecret = testProperty "Easy Secret" $ case sec of
    Secret (Location os' _, Variable "a") (_, p) | os == os' && os' == Parties (Set.singleton p1) && p == p1 -> True
    _ -> error (pretty sec)
  where sec' = runParser expressionParser empty "hardcoded example" "a = SECRET @P1"
        sec :: Statement Located
        (Location os _, sec) = case sec' of
          Right r -> r
          Left e -> error $ show e

testEasyFlip :: Test
testEasyFlip = testProperty "Easy Flip" $ case sec of
    Flip (Location os' _, Variable "a") (_, p) | os == os' && os' == Parties (Set.singleton p1) && p == p1 -> True
    _ -> error (pretty sec)
  where sec' = runParser expressionParser empty "hardcoded example" "a = FLIP @P1"
        sec :: Statement Located
        (Location os _, sec) = case sec' of
          Right r -> r
          Left e -> error $ show e

testEasySend :: Test
testEasySend = testProperty "Easy Send" $ case sec of
    Send (Location o2' _, p2s) (Location o1' _, Variable "a")
      | [p1] == (nub . concat $ Set.toList . parties <$> [o2', p2s])
        && [p2] == (nub . concat $ Set.toList . parties <$> [o1'])
        && [p1, p2] == (nub . concat $ Set.toList . parties <$> [o2]) -> True
    _ -> error (show sec)
  where sec' = runParser expressionParser (singleton (Variable "a") (Parties $ Set.singleton p2)) "hardcoded example" "SEND a TO P1"
        sec :: Statement Located
        (Location o2 _, sec) = case sec' of
          Right r -> r
          Left e -> error $ show e

testEasyObliv :: Test
testEasyObliv = testProperty "Easy Obliv" $ case sec of
    Oblivious (Location bindOs _, boundVar)
              (Location forOs _, forPs)
              (Location chooseOs _, choVar)
              ((Location frVTOs _, tVar), (Location frVFOs _, fVar))
      | [p2] == (nub . concat $ Set.toList . parties <$> [bindOs, forOs, forPs, chooseOs, o2])
        && null (nub . concat $ Set.toList . parties <$> [frVFOs, frVTOs])
        && [Variable "a"] == nub [choVar, fVar, tVar]
        && Variable "b" == boundVar
        -> True
    _ -> error (show sec)
  where sec' = runParser expressionParser (singleton (Variable "a") top) "hardcoded example" "b = OBLIVIOUSLYBY a CHOOSE a a FOR P2"
        sec :: Statement Located
        (Location o2 _, sec) = case sec' of
          Right r -> r
          Left e -> error $ show e

testLines :: Test
testLines = testProperty "Test multi-line program" $ 3 === length program
  where program :: Program Located
        program = either (error . show) id program'
        program' = runParser programParser empty "hardcode example" "\
        \a = 1\n\
        \b = 1\n\
        \c = 0"

testOutput :: Test
testOutput = testProperty "Test Output" $ (outputs, views) === deterministicEvaluation program inputs tapes
  where program :: Program Located
        Right program = runParser programParser empty "hardcode example" "\
        \a = SECRET @P2\n\
        \OUTPUT a"
        inputs = Inputs $ singleton (Variable "a") True
        tapes = Tapes empty
        views = Views empty
        outputs = Outputs $ singleton p2 $ singleton (Variable "a") True

testSend :: Test
testSend = testProperty "Test Send" $ (outputs, views) === deterministicEvaluation program inputs tapes
  where program :: Program Located
        Right program = runParser programParser empty "hardcode example" "\
        \a = 1\n\
        \SEND a TO P2"
        inputs = Inputs empty
        tapes = Tapes empty
        views = Views $ singleton p2 $ singleton (Variable "a") True
        outputs = Outputs empty

testObliv :: Test
testObliv = testProperty "Test Obliv" $ do -- the gen monad
  b <- arbitrary @Bool
  let program' = runParser programParser empty "hardcode example" $ "\
        \a = " ++ pretty b ++ "\n\
        \yes = 1\n\
        \no = 0\n\
        \b = OBLIVIOUSLYBY a CHOOSE yes no FOR P2\n\
        \OUTPUT b"
  let program = either (error . show) id program'
  let outputs = Outputs $ singleton p2 $ singleton (Variable "b") b
  let (observed, _) = deterministicEvaluation program (Inputs empty) (Tapes empty)
  return $ outputs === observed

example :: Test
example = testProperty "An easy example" $ (outputs, views) === deterministicEvaluation program inputs tapes
  where program :: Program Located
        Right program = runParser programParser empty "hardcode example" "\
        \rand = FLIP @P1\n\
        \sec = SECRET @P2\n\
        \comp1 = !(sec + 1)\n\
        \SEND comp1 TO P1\n\
        \y = rand ^ comp1\n\
        \OUTPUT y"
        {-program' = [(0, Variable p1 "rand", Flip p1)
                  ,(1, Variable p2 "sec", Secret p2)
                  ,(2, Variable p2 "comp1", Compute $ Not $ Xor (Var $ Variable p2 "sec") (Literal p2 True))
                  ,(3, Variable p1 "m1", Send p1 $ Var $ Variable p2 "comp1")
                  ,(4, Variable p1 "y", Output $ And (Var $ Variable p1 "rand") (Var $ Variable p1 "m1"))]-}
        inputs = Inputs $ singleton (Variable "sec") True
        tapes = Tapes $ singleton (Variable "rand") True
        views = Views $ fromList [(p1, fromList[(Variable "comp1", True),
                                                (Variable "rand", True)] )]
        outputs = Outputs $ singleton p1 $ singleton (Variable "y") True

pipeline :: Test
pipeline = testProperty "3partyXOR.cho gives correct outputs" $ ioProperty do
  program' <- parseFromFile (changeState (const ()) (const empty) programParser) "examples/3partyXOR.cho"
  let program = either (error . show) id program'
  return do  -- The Gen Monad!
    let (c, h1, h2) = (Party "C", Party "H1", Party "H2")
    secrets <- vectorOf 3 (arbitrary @Bool)
    let inputs = Inputs $ fromList $ [Variable "c_in"
                                     ,Variable "h1_in"
                                     ,Variable "h2_in"] `zip` secrets
    randomness <- vectorOf 6 (arbitrary @Bool)
    let tapes = Tapes $ fromList $ [Variable "c_s1"
                                   ,Variable "c_s2"
                                   ,Variable "h1_s1"
                                   ,Variable "h1_s2"
                                   ,Variable "h2_s1"
                                   ,Variable "h2_s2"] `zip` randomness
    let y = foldl (/=) False secrets
    let outputs = Outputs $ fromList $ [c, h1, h2] `zip` repeat (singleton (Variable "y") y)
    let (observed, _) = deterministicEvaluation program inputs tapes
    return $ observed === outputs

oneOfFourOT :: Test
oneOfFourOT = testProperty "1-of-4 Oblivious Transfer" $ ioProperty oneOfFourOTIO
oneOfFourOTIO :: IO (Gen Property)
oneOfFourOTIO = do
  program' <- parseFromFile (changeState (const ()) (const empty) programParser) "examples/1of4OT.cho"
  let program = either (error . show) id program'
  return do  -- The Gen Monad!
    messages <- vectorOf 4 (arbitrary @Bool)
    selection <- chooseInt (0,3)
    let sAsBits = [odd selection, 2 <= selection]
    let inputs = Inputs $ fromList $ [Variable "option_00"
                                     ,Variable "option_01"
                                     ,Variable "option_10"
                                     ,Variable "option_11"
                                     ,Variable "choice_i1"
                                     ,Variable "choice_i2"] `zip` (messages <> sAsBits)
    randomness <- vectorOf 4 (arbitrary @Bool)
    let tapes = Tapes $ fromList $ [Variable "k1_0"
                                   ,Variable "k1_1"
                                   ,Variable "k2_0"
                                   ,Variable "k2_1"] `zip` randomness
    let y = messages !! selection
    let outputs = Outputs $ fromList $ [p2] `zip` repeat (singleton (Variable "final") y)
    let (vc, (observed, _)) = deterministicEvaluation' program inputs tapes
    return $ counterexample (pretty vc) $ observed == outputs

