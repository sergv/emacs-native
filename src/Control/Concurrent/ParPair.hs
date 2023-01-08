----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.ParPair
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}

module Control.Concurrent.ParPair
  ( HasLength(..)
  , ParPair(..)

  , Sequential
  ) where

import Control.Concurrent.Async
import Control.Concurrent.Counter (Counter)
import Control.Concurrent.Counter qualified as Counter
import Data.Vector.Generic.Mutable qualified as GM

class HasLength a where
  getLength :: a -> Int

instance GM.MVector v a => HasLength (v s a) where
  {-# INLINE getLength #-}
  getLength = GM.length

class ParPair a init m | a -> init where
  initialize :: init -> m a
  parPair
    :: (HasLength b, HasLength d)
    => a -> (b -> m c) -> (d -> m e) -> b -> d -> m (c, e)

-- | No parallelism
data Sequential = Sequential

instance Monad m => ParPair Sequential () m where
  {-# INLINE initialize #-}
  initialize _ = pure Sequential
  {-# INLINE parPair #-}
  parPair _ f g b d = (,) <$> f b <*> g d

-- | At most N concurrent jobs
newtype Parallel = Parallel Counter

instance ParPair Parallel Int IO where
  {-# INLINE initialize #-}
  initialize = fmap Parallel . Counter.new
  {-# INLINE parPair #-}
  parPair (Parallel sync) f g b d
    | getLength b > 1000
    = do
      old <- Counter.sub sync 1
      if old <= 0
      then do
        _ <- Counter.add sync 1
        (,) <$> f b <*> g d
      else
        withAsync (f b <* Counter.add sync 1) $ \job -> do
          d' <- g d
          (, d') <$> wait job
    | otherwise
    = (,) <$> f b <*> g d

