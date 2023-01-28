----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.Fork
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}

module Control.Concurrent.Fork
  ( HasLength(..)
  , Fork(..)

  , Sequential(..)
  , Parallel(..)
  , mkParallel
  ) where

-- import Control.Concurrent.Counter (Counter)
-- import Control.Concurrent.Counter qualified as Counter
-- import Control.Concurrent

import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Coerce
import Data.Vector.Generic.Mutable qualified as GM
import Debug.Trace (traceEventIO)

class HasLength a where
  getLength :: a -> Int

instance GM.MVector v a => HasLength (v s a) where
  {-# INLINE getLength #-}
  getLength = GM.length

-- | Parallel operation for the quicksort algorithm. Tells how to
-- apply a pair of functions to their respective inputs
class Fork a x m | a -> x where
  startWork :: a -> m x
  endWork   :: a -> x -> m ()
  fork
    :: (HasLength b, HasLength d)
    => a -> Maybe x -> (Maybe x -> b -> m c) -> (Maybe x -> d -> m e) -> b -> d -> m (c, e)

-- | No parallelism
data Sequential = Sequential

instance Monad m => Fork Sequential () m where
  {-# INLINE startWork #-}
  {-# INLINE endWork   #-}
  {-# INLINE fork      #-}
  startWork _ = pure ()
  endWork _ _ = pure ()
  fork _ _ f g b d = (,) <$> f Nothing b <*> g Nothing d



newtype Semaphore = Semaphore (TVar Int)

newSemaphore :: Int -> IO Semaphore
newSemaphore = coerce . newTVarIO

acquire :: Semaphore -> IO ()
acquire (Semaphore v) = atomically $ do
  !n <- readTVar v
  if n > 0
  then writeTVar v $! n - 1
  else retry

release :: Semaphore -> IO ()
release (Semaphore v) = atomically $ do
  !n <- readTVar v
  writeTVar v $! n + 1


-- | At most N concurrent jobs at any given time.
-- newtype Parallel = Parallel Counter
-- newtype Parallel = Parallel (TVar Int)
newtype Parallel = Parallel Semaphore

mkParallel :: Int -> IO Parallel
mkParallel hint =
  coerce (newSemaphore hint)
  -- coerce (Counter.new hint)
  -- coerce (Counter.new 0)
  -- coerce (newTVarIO (0 :: Int))

instance Fork Parallel () IO where
  {-# INLINE startWork #-}
  {-# INLINE endWork   #-}
  {-# INLINE fork      #-}
  startWork (Parallel s) = do
    -- tid <- myThreadId
    -- putStrLn $ "Start work, tid = " ++ show tid
    traceEventIO "thread started"
    acquire s
    traceEventIO "semaphore acquired"
  endWork (Parallel s) _ = do
    release s
    traceEventIO "semaphore released"

  fork _ Nothing f g b d
    = (,) <$> f Nothing b <*> g Nothing d
  fork p tok@(Just releaseToken) f g b d
    | bigB && bigD
    = withAsync (startWork p >>= \token -> f (Just token) b) $ \jobB -> do
        withAsync (startWork p >>= \token -> g (Just token) d) $ \jobD -> do
          endWork p releaseToken
          traceEventIO "waiting on results"
          res <- (,) <$> wait jobB <*> wait jobD
          traceEventIO "waiting on results - done"
          pure res
    | bigB
    = withAsync (startWork p >>= \token -> f (Just token) b) $ \jobB -> do
        d' <- g tok d
        traceEventIO "waiting on results"
        b' <- wait jobB
        traceEventIO "waiting on results - done"
        pure (b', d')
    | bigD
    = withAsync (startWork p >>= \token -> g (Just token) d) $ \jobD -> do
        b' <- f tok b
        traceEventIO "waiting on results"
        d' <- wait jobD
        traceEventIO "waiting on results - done"
        pure (b', d')
    | otherwise
    = (,) <$> f Nothing b <*> g Nothing d <* endWork p releaseToken

    where
      bigB = getLength b > 1000
      bigD = getLength d > 1000

  -- fork (Parallel sync) f g b d
    -- | getLength b > 1000
    -- = do
    --   old <- Counter.sub sync 1
    --   if old <= 0
    --   then do
    --     _ <- Counter.add sync 1
    --     (,) <$> f b <*> g d
    --   else
    --     withAsync (f b <* Counter.add sync 1) $ \job -> do
    --       d' <- g d
    --       -- Let others run for a while since current thread is blocked
    --       -- waiting.
    --       _  <- Counter.add sync 1
    --       b' <- wait job
    --       -- Restore the balance
    --       _  <- Counter.sub sync 1
    --       pure (b', d')
    -- | otherwise
    -- = (,) <$> f b <*> g d

-- instance Fork Parallel IO where
--   {-# INLINE fork #-}
--   fork (Parallel sync) f g b d = do
--     -- atomically $ modifyTVar' sync (\x -> x + 1)
--     _old <- Counter.add sync 1
--     (,) <$> f b <*> g d
--     -- | getLength b > 1000
--     -- = do
--     --   -- _old <- Counter.sub sync 1
--     --   atomically $ modifyTVar' sync (\x -> x + 1)
--     --   (\y x -> (x, y)) <$> g d <*> f b
--     --   -- old <- Counter.sub sync 1
--     --   -- if old <= 0
--     --   -- then do
--     --   --   _ <- Counter.add sync 1
--     --   --   (,) <$> f b <*> g d
--     --   -- else
--     --   --   error "never happens"
--     --     -- withAsync (f b) $ \job -> do
--     --     --   d' <- g d
--     --     --   -- Let others run for a while since current thread is blocked
--     --     --   -- waiting.
--     --     --   -- _  <- Counter.add sync 1
--     --     --   b' <- wait job
--     --     --   -- Restore the balance
--     --     --   -- _  <- Counter.sub sync 1
--     --     --   pure (b', d')
--     -- | otherwise
--     -- = (,) <$> f b <*> g d


