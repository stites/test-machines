{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
module Control.Mealy
  ( MealyT
  , runMealyT
  , delay
  , stateful
  , statefulM
  , accum
  , countdown
  , (Control.Category..)
  )  where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Category
import Control.Monad.Fix

newtype MealyT m a b = MealyT
  { runMealyT :: a -> m (b, MealyT m a b) }

delay :: Monad m => a -> MealyT m a a
delay x = MealyT $ \a -> return (x, delay a)

accum :: Monad m => (b -> a -> b) -> b -> MealyT m a b
accum f = stateful (\a b -> let b' = f b a in (b', b'))

instance Functor m => Functor (MealyT m x) where
  fmap :: (a -> b) -> MealyT m x a -> MealyT m x b
  fmap fab ma = MealyT $ \x -> (fab *** fmap fab) <$> runMealyT ma x

instance Monad m => Applicative (MealyT m x) where
  -- pure returns a const mealy
  pure :: a -> MealyT m x a
  pure a = MealyT . const . pure $ (a, pure a)

  (<*>) :: MealyT m x (a -> b) -> MealyT m x a -> MealyT m x b
  mFab <*> ma = MealyT $ \x ->
    runMealyT mFab x >>= \(fab, mFab') ->
      runMealyT ma x >>= \(a, ma') ->
        return (fab a, mFab' <*> ma')
--        f (f, Mf) (a, Ma) -> (f a, Mf <*> Ma)
--        pure f <*> rM <*> rM
--        == f <$> rM <*> rM

instance Monad m => Category (MealyT m) where
  id :: MealyT m a a
  id = MealyT $ \a -> pure (a, id)

  (.) :: MealyT m b c -> MealyT m a b -> MealyT m a c
  mbc . mab = MealyT $ \a -> do
    (b, mab') <- runMealyT mab a
    (c, mbc') <- runMealyT mbc b
    return (c, mbc' . mab')

instance Monad m => Arrow (MealyT m) where
  arr :: (a -> b) -> MealyT m a b
  arr f = MealyT $ \a -> return (f a, arr f)

  first :: MealyT m a b -> MealyT m (a, x) (b, x)
  first mab = MealyT $ \(a, x) -> do
    (b, mab') <- runMealyT mab a
    return ((b, x), first mab')

-- common monad anti-pattern
-- (flip fmap) (runMealyT mab a) ((,x) *** first)

instance Monad m => ArrowChoice (MealyT m) where
  left :: MealyT m a b -> MealyT m (Either a x) (Either b x)
  left mab = MealyT $ \case
    Left a -> do
      (b, mab') <- runMealyT mab a
      return (Left b, left mab')
    Right x -> return (Right x, left mab)

instance MonadFix m => ArrowLoop (MealyT m) where
  loop :: MealyT m (a,x) (b,x) -> MealyT m a b
  loop maxbx = MealyT $ \a -> mdo
    ((b, x), maxbx') <- runMealyT maxbx (a, x)
    return (b, loop maxbx')

stateful :: Applicative m => (a -> s -> (b, s)) -> s -> MealyT m a b
stateful f s = MealyT $ \a ->
  let
    (b, s') = f a s
  in
    pure (b, stateful f s')

statefulM :: Monad m => (a -> s -> m (b, s)) -> s -> MealyT m a b
statefulM f s = MealyT $ \a -> do
  (b, s') <- f a s
  pure (b, statefulM f s')

-------------------------------------------------------------------------------
-- Mealy Machine runner
-------------------------------------------------------------------------------
countdown :: (count ~ Integer, Monad m) => count -> MealyT m () b -> m ()
countdown i meal
  | i <= 0 = pure ()
  | otherwise = do
    (_, meal') <- runMealyT meal ()
    countdown (i-1) meal'

