---------------------------------------------------------------------------------
-- |
-- A pipe which produces a long data-file consisting of a long repetition
-- of the same symbol, delimited by a given parameter
-- To be used in conjunction with CSSR.
--
-- First argument is the symbol, the second argument is the number of
-- repetitions
-- Note that if the output "symbol" is itself a string, it will produce copies
-- of that template.
---------------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Machines.AllSameSymbol
  ( runAllSame
  ) where

import Prelude
import Data.Text (Text)
import Data.Monoid ((<>))
import GHC.Natural
import Pipes
-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text as T
-- >>> let out = (lift . putStr . T.unpack)


-- |
--
-- Examples:
--
-- >>> runEffect (Pipes.for (allSame "foo" "<>" 1) out)
-- foo
-- >>> runEffect (Pipes.for (allSame "foo" "bar" 2) out)
-- foobarfoo
allSame :: forall m . Monad m => Text -> Text -> Natural -> Producer' Text m ()
allSame symbol delim counts = go counts
  where
    go :: Natural -> Producer' Text m ()
    go 0 = yield ("\n")
    go 1 = yield (symbol <> "\n")
    go c = yield (symbol <> delim) >> go (c-1)


-- |
--
-- Examples:
--
-- >>> let out = (lift . putStr . T.unpack)
-- >>> runAllSame "foo" "<>" 0 out
-- <BLANKLINE>
-- >>> runAllSame "foo" "<>" 1 out
-- foo
-- >>> runAllSame "foo" "<>" 2 out
-- foo<>foo
-- >>> runAllSame "" "" 2 out
-- <BLANKLINE>
-- >>> runAllSame "" "," 2 out
-- ,
-- >>> runAllSame "oink" "," 3 out
-- oink,oink,oink
runAllSame :: Monad m => Text -> Text -> Natural -> (Text -> Effect m ()) -> m ()
runAllSame s d c consumer = runEffect $ Pipes.for (allSame s d c) consumer

