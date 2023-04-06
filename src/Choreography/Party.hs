module Choreography.Party
where


newtype Party = Party String
           deriving (Eq, Ord)

instance Show Party where  -- not really proper, but I need it for sanity.
  show (Party name) = name

p1 :: Party
p1 = Party "P1"  -- "ℙ𝟙"
p2 :: Party
p2 = Party "P2"  -- "ℙ𝟚"
p3 :: Party
p3 = Party "P3"  -- "ℙ𝟛"
