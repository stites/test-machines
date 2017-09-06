-- Program which produces a long data-file consisting of a IID tosses of
-- a biased coin, bias set by input.
-- The output alphabet is 0, 1.
-- The output symbols are unspaced.
-- To be used in conjunction with Kris's state-inference program.
--
-- First argument is the probability of a 1, the second argument is the number
-- of repetitions
-- Output is to std-out, use redirects to put in files.
module Test.Machines.BiasedCoin where

import Data.Proxy
import Prelude
import Data.Text (Text)
import qualified Data.Text as T
import Control.Exception
import System.Random
import Test.Machines
import Test.Machines.Internal


data SingleCoin = SingleCoin
  deriving (Eq, Ord, Show)

coinProxy :: Proxy SingleCoin
coinProxy = Proxy

-- TODO: Make this dynamically customizable
instance SeriesGenerating SingleCoin where
  type Symbol SingleCoin = BinaryOutput
  initial = SingleCoin

  move SingleCoin _ = SingleCoin

  distribution SingleCoin = [ (0.75, Z), (0.25, O) ]

