----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.EarlyTermination
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Emacs.EarlyTermination
  ( runWithEarlyTermination
  , processInputDelayMicroseconds
  , consumeTMQueueWithEarlyTermination
  ) where

import Control.Concurrent.Async.Lifted.Safe
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Control.Monad.Base
import Control.Monad.Trans.Control

import Control.Monad.EarlyTerminate

processInputDelayMicroseconds :: Int
processInputDelayMicroseconds = 100_000

{-# INLINE newDelay #-}
newDelay :: MonadBase IO m => m (TVar Bool)
newDelay = liftBase (registerDelay processInputDelayMicroseconds)

{-# INLINE runWithEarlyTermination #-}
runWithEarlyTermination
  :: forall m a. (MonadBaseControl IO m, Forall (Pure m), MonadEarlyTerminate m)
  => IO a
  -> m a
runWithEarlyTermination doWork =
  withAsync (liftBase doWork) $ \worker -> do
    let go :: TVar Bool -> m a
        go delayVar = do
          res <- liftBase $ atomically $
            (Left <$> (readTVar delayVar >>= check)) `orElse` (Right <$> waitSTM worker)
          case res of
            Right a -> pure a
            Left () -> earlyTerminationPoint $ go =<< newDelay
    go =<< newDelay

{-# INLINE consumeTMQueueWithEarlyTermination #-}
-- | Interleave reading from TMQueue and checking 'processInput'.
consumeTMQueueWithEarlyTermination
  :: forall m a b. (MonadBaseControl IO m, Forall (Pure m), MonadEarlyTerminate m)
  => TMQueue a -- ^ Source of items
  -> b        -- ^ Internal state
  -> (b -> a -> m b)
  -> m b
consumeTMQueueWithEarlyTermination !source !initState f =
  go initState =<< newDelay
  where
    go :: b -> TVar Bool -> m b
    go !initAcc delayVar = go' initAcc
      where
        go' !acc = do
          res <- liftBase $ atomically $
            (Left <$> (readTVar delayVar >>= check)) `orElse` (Right <$> readTMQueue source)
          case res of
            Right Nothing  -> pure acc
            Right (Just a) -> go' =<< f acc a
            Left ()        -> earlyTerminationPoint $ go acc =<< newDelay

