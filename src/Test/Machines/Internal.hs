module Test.Machines.Internal where

import Prelude

data BinaryOutput = Z | O

instance Show BinaryOutput where
  show Z = "0"
  show O = "1"

prob1Dist :: Float -> [(Float, BinaryOutput)]
prob1Dist p = [(1 - p, Z), (p, O)]

