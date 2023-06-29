{-# LANGUAGE QuasiQuotes #-}
module GMW where

import Text.Parsec (parse)

import Choreography (PartySet, Program, programParser, Located, Sourced)
import Circuit
import Utils (litFile)

header :: Program Sourced
Right header = parse programParser "_compileTime_:snippits/GMW_header.cho" [litFile|snippits/GMW_header.cho|]

gmw :: Circuits -> Program Located
gmw = undefined
