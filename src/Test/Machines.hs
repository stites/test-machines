---------------------------------------------------------------------------------
-- |
-- Module    :  Test.Machines
-- Copyright :  (c) Sam Stites 2016
-- License   :  MIT
-- Maintainer:  sam@stites.io
-- Stability :  experimental
-- Portability: non-portable
--
-- A small library to stochastically generate events according to given rules.
---------------------------------------------------------------------------------
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.Machines where

import Control.Arrow (arr)
import Control.Exception (bracket)
import Data.Monoid ((<>))
import Data.Proxy
import Data.Text (Text)
import System.IO
import System.Random
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import Prelude hiding ((.))
import Control.Mealy

class Show s => SeriesGenerating s where
  type Symbol s

  move :: s -> Symbol s -> s
  initial :: s
  distribution :: s -> [(Float, Symbol s)]

st2T :: (SeriesGenerating s, Show s) => s -> Text
st2T   = T.pack . show

ts2T :: (SeriesGenerating s, Show (Symbol s)) => Proxy s -> Symbol s -> Text
ts2T _ = T.pack . show

writeSeries :: forall s . (Show (Symbol s), SeriesGenerating s) => Proxy s -> Text -> FilePath -> Integer -> IO ()
writeSeries _ delim filename iters =
  bracket openTimeseq hClose nestedBracket
  where
    nestedBracket :: Handle -> IO ()
    nestedBracket ts = bracket openStateseq hClose (buildFiles ts)

    openTimeseq :: IO Handle
    openTimeseq = openFile (filename ++ "_timeseq") WriteMode

    openStateseq :: IO Handle
    openStateseq = openFile (filename ++ "_stateseq") WriteMode

    buildFiles :: Handle -> Handle -> IO ()
    buildFiles ts st = do
      gen <- getStdGen
      countdown iters $ seriesMealy gen ts st delim (initial :: s)

-------------------------------------------------------------------------------
-- Mealy machines
-------------------------------------------------------------------------------

seriesMealy :: forall a s .(Show (Symbol s), SeriesGenerating s)
            => StdGen -> Handle -> Handle -> Text -> s -> MealyT IO a (Symbol s, s)
seriesMealy seed timeseq stateseq delim start = proc _ -> do
  (sym, state) <- meals -< ()
  _ <- writeShowable timeseq  -< (ts2T (Proxy :: Proxy s) sym <> delim)
  _ <- writeShowable stateseq -< (st2T state <> delim)
  arr id -< (sym, state)
  where
    meals :: MealyT IO () (Symbol s, s)
    meals = getTransition start . getProb seed


writeShowable :: Handle -> MealyT IO Text ()
writeShowable = statefulM step
  where
    step :: Text -> Handle -> IO ((), Handle)
    step s hdl = do
      TIO.hPutStr hdl s
      return ((), hdl)


getTransition :: forall s m . (SeriesGenerating s, Monad m)
              => s -> MealyT m Float (Symbol s, s)
getTransition = stateful step
  where
    step :: Float -> s -> ((Symbol s, s), s)
    step p state = let res@(_, s) = transition p state in (res, s)

    transition :: Float -> s -> (Symbol s, s)
    transition p s = (emitted, move s emitted)
      where
        emitted :: Symbol s
        emitted = snd . head . dropWhile ((< p) . fst) . cdf $ s

        cdf :: s -> [(Float, Symbol s)]
        cdf = scanl1 merge . distribution

        merge :: Num n => (n, a) -> (n, a) -> (n, a)
        merge (p0, _) (p1, s') = (p0 + p1, s')


getProb :: Monad m => StdGen -> MealyT m () Float
getProb = stateful step
  where
    step :: () -> StdGen -> (Float, StdGen)
    step () = randomR (0, 1)


