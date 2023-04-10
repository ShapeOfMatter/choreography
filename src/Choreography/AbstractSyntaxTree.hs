module Choreography.AbstractSyntaxTree
where

import Choreography.Party (Party(..))
import Utils (LineNo)


data Variable = Variable {party :: Party, variable :: String}
              deriving (Eq, Ord, Show)

data Algebra = Literal Party Bool
             | Var Variable
             | Xor Algebra Algebra
             | And Algebra Algebra
             | Not Algebra
             deriving (Show)

data Expression = Compute Algebra
                | Secret Party
                | Flip Party
                | Send Party Algebra
                | Output Algebra
                deriving (Show)

type Program = [(LineNo, Variable, Expression)]
