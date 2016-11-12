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
module EvenProcess where

import Data.Proxy
import Data.Text (Text)
import Control.Exception
import qualified Data.Text as T

import Prelude
import Test.Machines

data EvenProcess = A | B
  deriving (Eq, Ord, Show)

evenProxy :: Proxy EvenProcess
evenProxy = Proxy

instance SeriesGenerating EvenProcess where
  type Symbol EvenProcess = Text
  initial = A

  move A "0" = A
  move A "1" = B
  move B "0" = A
  move B "1" = B
  move _ _ = throw $
    PatternMatchFail "illegal match - check your state machine definition"


  distribution A = [ (0.5, "0"), (0.5, "1") ]
  distribution B = [ (1.0, "0"), (0.0, "1") ]

  st2T = T.pack . show
  ts2T _ = id



