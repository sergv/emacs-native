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
  ( EarlyTermination(..)
  , runWithEarlyTermination
  , processInputDelayMicroseconds
  , consumeTMQueueWithEarlyTermination
  ) where

import Control.Concurrent.Async.Lifted.Safe
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Control.Exception
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Prettyprinter

import Data.Emacs.Module.Env.ProcessInput qualified as ProcessInput
import Emacs.Module

-- | User requested to terminate computation early via C-g.
data EarlyTermination = EarlyTermination
  deriving (Show)

instance Exception EarlyTermination

instance Pretty EarlyTermination where
  pretty EarlyTermination = "EarlyTermination"

processInputDelayMicroseconds :: Int
processInputDelayMicroseconds = 100_000

{-# INLINE mkDelay #-}
mkDelay :: MonadBase IO m => m (TVar Bool)
mkDelay = liftBase (registerDelay processInputDelayMicroseconds)

{-# INLINE runWithEarlyTermination #-}
runWithEarlyTermination
  :: forall m v s a.
     ( MonadEmacs m v
     , MonadBaseControl IO (m s)
     , Forall (Pure (m s))
     , forall ss. MonadThrow (m ss)
     )
  => IO a
  -> m s a
runWithEarlyTermination doWork =
  withAsync (liftBase doWork) $ \worker -> do
    let go :: TVar Bool -> m s a
        go delayVar = do
          res <- liftBase $ atomically $
            (Left <$> (readTVar delayVar >>= check)) `orElse` (Right <$> waitSTM worker)
          case res of
            Right a -> pure a
            Left () ->
              processInput >>= \case
                ProcessInput.Quit     -> throwM EarlyTermination
                ProcessInput.Continue -> go =<< mkDelay
    go =<< mkDelay

{-# INLINE consumeTMQueueWithEarlyTermination #-}
-- | Interleave reading from TMQueue and checking 'processInput'.
consumeTMQueueWithEarlyTermination
  :: forall m v s a b.
     ( MonadEmacs m v
     , MonadBaseControl IO (m s)
     , Forall (Pure (m s))
     , forall ss. MonadThrow (m ss)
     )
  => TMQueue a -- ^ Source of items
  -> b        -- ^ Internal state
  -> (b -> a -> m s b)
  -> m s b
consumeTMQueueWithEarlyTermination !source !initState f =
  go initState =<< mkDelay
  where
    go :: b -> TVar Bool -> m s b
    go !acc delayVar = do
      res <- liftBase $ atomically $
        (Left <$> (readTVar delayVar >>= check)) `orElse` (Right <$> readTMQueue source)
      case res of
        Right Nothing  -> pure acc
        Right (Just a) -> do
          acc' <- f acc a
          go acc' delayVar
        Left () ->
          processInput >>= \case
            ProcessInput.Quit     -> throwM EarlyTermination
            ProcessInput.Continue -> go acc =<< mkDelay

