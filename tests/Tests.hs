module Tests where


import Distribution.TestSuite.QuickCheck

tests :: IO [Test]
tests = return [tautology]


tautology :: Test
tautology = testProperty "Tautology" \i -> (==) @Int i i
