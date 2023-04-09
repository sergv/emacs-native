----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Utils
-- Copyright   :  (c) Sergey Vinokurov 2023
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Emacs.Utils
  ( EarlyTermination(..)
  , runWithEarlyTermination
  ) where

import Control.Concurrent
import Control.Concurrent.Async.Lifted.Safe
import Control.Exception
import Control.Monad
import Control.Monad.Base
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

runWithEarlyTermination
  :: forall m v s a. (MonadEmacs m v, MonadBaseControl IO (m s), Forall (Pure (m s)), MonadThrow (m s))
  => m s a
  -> m s a
runWithEarlyTermination doWork =
  withAsync doWork $ \worker -> do
    let processAborts :: m s b
        processAborts = forever $ do
          res <- processInput
          case res of
            ProcessInput.Continue -> pure ()
            ProcessInput.Quit     -> throwM EarlyTermination
          liftBase $ threadDelay 100_000

    withAsync processAborts $ \aborts -> do
      -- Will rethrow EarlyTermination if user aborted.
      waitEither aborts worker >>= either pure pure
