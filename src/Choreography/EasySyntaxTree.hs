module Choreography.EasySyntaxTree
where

import Choreography.Party (Party(..))


type Variable = String

data Algebra = Literal Bool
             | Var Variable
             | Xor Algebra Algebra
             | And Algebra Algebra
             | Not Algebra
             deriving (Show)

data Expression = Compute Algebra
                | Secret
                | Flip
                | Send Party Algebra
                | Output Algebra
                | Annotated Party Expression
                deriving (Show)

newtype Program = Program [(Variable, Expression)] deriving (Show)
