module Choreography.Flatten where

import Data.Map.Strict (insertWith, Map)
import Polysemy (Members, run, Sem)
import Polysemy.State (modify, State)

import Choreography.AbstractSyntaxTree
import Choreography.Parser (Sourced)
import Choreography.Party

flatten :: PreProgram Sourced -> Program Sourced
flatten = concat . run . undefined . traverse flattenLine

flattenLine :: forall r.
               (Members '[State (Map Variable Variable), State (Map Party Party), State (Map String (Program Sourced))] r) =>
               Sourced (PreProgramLine Sourced) -> Sem r (Program Sourced)
flattenLine (source, PPStmnt stmnt) = return [(source, stmnt)]
flattenLine (_, PPMacro Macro{name, body}) = do modify $ insertWith (<>) name body
                                                return []
flattenLine (_, PPCall MacroCall{macro, variables, aliases, returns}) = 
