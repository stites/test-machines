module EvenProcess where

import Data.Proxy
import Data.Text (Text)
import Control.Exception
import qualified Data.Text as T

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



