---------------------------------------------------------------------------------
-- |
-- Program which produces a long data-file simulating the strictly sofic system
-- known as the even process.
--
-- The process produces strings of ones and zeros, subject to the constraints
-- that ones are produced in blocks of even length.  Thus, in state A, we may
-- produce a 0, which leads back to A, or we may produce a one, which leads to B.
-- In B we must produce a 1 and return to A.
--
-- The probability of emitting a 0 in state A is 0.5
--
-- For the even process:
--  + We always begin in state A.
--  + This takes, as input, the number of steps to simulate.
--  + The output symbols are unspaced by default.
--  + Output is to EP_timeseq
--  + [ ] Should also output a machine file, EP_machine
--  + To be used in conjunction with CSSR
--
---------------------------------------------------------------------------------
module Test.Machines.EvenProcess where

import Prelude
import Data.Proxy

import Test.Machines
import Test.Machines.Internal

data EvenProcess = A | B
  deriving (Eq, Ord, Show)

evenProxy :: Proxy EvenProcess
evenProxy = Proxy

instance SeriesGenerating EvenProcess where
  type Symbol EvenProcess = BinaryOutput
  initial = A

  move A Z = A
  move A O = B
  move B Z = A
  move B O = B

  distribution A = [ (0.5, Z), (0.5, O) ]
  distribution B = [ (1.0, Z), (0.0, O) ]



