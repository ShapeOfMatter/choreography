module Choreography.AbstractSyntaxTree
where

import Data.Map.Strict (Map)
import Text.Parsec (SourcePos)

import Choreography.Party (Party(..), PartySet)
import Utils (Pretty, pretty, Pretty1)


newtype Variable = Variable {variable :: String} deriving (Eq, Ord, Show)
instance Pretty Variable where pretty = variable

newtype Bit = Bit { bit :: Bool } deriving (Bounded, Enum, Eq, Ord, Show)
trueNames :: [String]
trueNames = ["1", "true"]
falseNames :: [String]
falseNames = ["0", "false"]
instance Pretty Bit where
  pretty (Bit True) = head trueNames
  pretty (Bit False) = head falseNames

data Algebra f = Literal (f Bit)
               | Var (f Variable)
               | Xor (f (Algebra f)) (f (Algebra f))
               | And (f (Algebra f)) (f (Algebra f))
               | Not (f (Algebra f))
deriving instance (forall a. (Show a) => Show (f a)) => Show (Algebra f)
xorNames :: [String]
xorNames = ["⊕", "XOR", "+", "<>", "!=", "⊻"]
andNames :: [String]
andNames = ["∧", "AND", "*", "^"]
notNames :: [String]
notNames = ["¬", "NOT", "!", "~"]
instance (Pretty1 f, Functor f) => Pretty (Algebra f) where
  pretty (Literal fb) = pretty fb
  pretty (Var fv) = pretty fv
  pretty (Xor fa1 fa2) = unwords [pretty fa1, head xorNames, pretty fa2]
  pretty (And fa1 fa2) = unwords [pretty fa1, head andNames, pretty fa2]
  pretty (Not fa) = unwords [head notNames, pretty fa]

data ObvBody f = ObvBody (f (ObvChoice f)) (f (ObvChoice f)) (f Variable)
deriving instance (forall a. (Show a) => Show (f a)) => Show (ObvBody f)
data ObvChoice f = ObvLeaf Variable
                 | ObvBranch (ObvBody f)
deriving instance (forall a. (Show a) => Show (f a)) => Show (ObvChoice f)
oblivSymbols :: [String]
oblivSymbols = ["[ ", ", ", " ]"]
choiceKeyword :: String
choiceKeyword = "?"
instance (Pretty1 f, Functor f) => Pretty (ObvBody f) where
  pretty (ObvBody fc0 fc1 fv) = head oblivSymbols ++ pretty fc0 ++ oblivSymbols !! 1 ++ pretty fc1 ++ oblivSymbols !! 2
                                ++ choiceKeyword ++ pretty fv
instance (Pretty1 f, Functor f) => Pretty (ObvChoice f) where
  pretty (ObvLeaf v) = pretty v
  pretty (ObvBranch body) = pretty body

data Statement f = Compute (f Variable) (f (Algebra f))
                 | Secret (f Variable) (f Party)
                 | Flip (f Variable) (f Party)
                 | Send (f PartySet) (f Variable)
                 | Oblivious (f Variable) (f PartySet) (f (ObvBody f))
                 | Output (f Variable)
deriving instance (forall a. (Show a) => Show (f a)) => Show (Statement f)
bindKeyword :: String
bindKeyword = "="
atKeyword :: String
atKeyword = "@"
secretKeyword :: String
secretKeyword = "SECRET"
flipKeyword :: String
flipKeyword = "FLIP"
sendKeywords :: [String]
sendKeywords = ["SEND", "TO"]
oblivKeywords :: [String]
oblivKeywords = ["OBLIVIOUSLY", "FOR"]
outputKeyword :: String
outputKeyword = "OUTPUT"
instance (Pretty1 f, Functor f) => Pretty (Statement f) where
  pretty (Compute fv fa) = unwords [pretty fv, bindKeyword, pretty fa]
  pretty (Secret fv fp) = unwords [pretty fv, bindKeyword, secretKeyword, atKeyword, pretty fp]
  pretty (Flip fv fp) = unwords [pretty fv, bindKeyword, flipKeyword, atKeyword, pretty fp]
  pretty (Send fps fv) = unwords [head sendKeywords,
                                  pretty fv,
                                  sendKeywords !! 1,
                                  pretty fps]
  pretty (Oblivious fv fps fb) = unwords [pretty fv,
                                      pretty bindKeyword,
                                      head oblivKeywords,
                                      pretty fb,
                                      oblivKeywords !! 1,
                                      pretty fps]
  pretty (Output fv) = unwords [outputKeyword, pretty fv]

type Program f = [f (Statement f)]

data Macro f = Macro {name :: String, body :: Program f}
deriving instance (forall a. (Show a) => Show (f a)) => Show (Macro f)
macroKeywords :: [String]
macroKeywords = ["MACRO", "AS", "ENDMACRO"]

data MacroCall f = MacroCall {macro :: String,
                              variables :: Map Variable (f Variable),
                              aliases :: Map Party (f Party),
                              returns :: Map Variable (f Variable)}
deriving instance (forall a. (Show a) => Show (f a)) => Show (MacroCall f)
callKeywords :: [String]
callKeywords = ["DO", "USING", "ANDUSING", "RETRIEVING"]
callSymbols :: [String]
callSymbols = ["[", "=", ",", "]"]

data PreProgramLine f = PPStmnt (Statement f)
                      | PPMacro (Macro f)
                      | PPCall (MacroCall f)
deriving instance (forall a. (Show a) => Show (f a)) => Show (PreProgramLine f)

type PreProgram f = [f (PreProgramLine f)]


gatherSelectionVars :: (Foldable f) => ObvBody f -> [Variable]
gatherSelectionVars (ObvBody fc0 fc1 fv) = concatMap (:[]) fv <> concatMap gBranch fc0 <> concatMap gBranch fc1
  where gBranch (ObvLeaf _) = []
        gBranch (ObvBranch body) = gatherSelectionVars body


class Proper f where
  owners :: f a -> PartySet
  value :: f a -> a
instance Proper ((,) PartySet) where
  owners = fst
  value = snd

data Location = Location { lowners :: PartySet, source :: SourcePos } deriving (Eq, Ord, Show)
type Located = (,) Location
instance Proper Located where
  owners = lowners . fst
  value = snd

data Improper = Improper { iowners :: Maybe PartySet, isource :: SourcePos } deriving (Eq, Ord, Show)
type ILocated = (,) Improper
