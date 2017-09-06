-- Program which produces a long data-file simulating Foulkes HMM
-- The model is constructed so that each state can be unambiguously identified
-- on the basis of the end of the observed history.
-- The states are, in an arbitrary order:
-- A: {*000}
-- B: {*0001}
-- C: {*11}
-- D: {*101}
-- E: {*10}
-- F: {*1001}
-- G: {*100}
--
-- This determines the topology of the system, but let's be explicit:
-- A, 0 -> A
-- A, 1 -> B
-- B, 0 -> E
-- B, 1 -> C
-- C, 0 -> E
-- C, 1 -> C
-- D, 0 -> E
-- D, 1 -> C
-- E, 0 -> G
-- E, 1 -> D
-- F, 0 -> E
-- F, 1 -> C
-- G, 0 -> A
-- G, 1 -> F
--
-- the probability of emitting a 1:
-- A = 13/16
-- B = 13/16
-- C = 1/16
-- D = 12/16
-- E = 7/16
-- F = 4/16
-- G = 7/16
--
-- We always begin in state A.
-- The first input is the number of steps to simulate.  That is the only
-- input.
-- The output symbols are unspaced.
-- Output is to files named Foulkes_foo.
-- To be used in conjunction with Kris's state-inference program.
module Test.Machines.Foulkes where

import Test.Machines.Internal

data FoulkesState = A | B | C | D | E | F | G
  deriving (Eq, Ord, Show)

class SeriesGenerating FoulkesState where
  type Symbol FoulkesState = BinaryOutput
  initial = A

  move A Z = A
  move A O = B
  move B Z = E
  move B O = C
  move C Z = E
  move C O = C
  move D Z = E
  move D O = C
  move E Z = G
  move E O = D
  move F Z = E
  move F O = C
  move G Z = A
  move G O = F

  distribution A = prob1Dist (13/16)
  distribution B = prob1Dist (13/16)
  distribution C = prob1Dist ( 1/16)
  distribution D = prob1Dist (12/16)
  distribution E = prob1Dist ( 7/16)
  distribution F = prob1Dist ( 4/16)
  distribution G = prob1Dist ( 7/16)

