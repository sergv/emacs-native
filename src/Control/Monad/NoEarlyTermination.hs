-- |
-- Module:     Control.Monad.NoEarlyTermination
-- Copyright:  (c) Sergey Vinokurov 2026
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE DerivingStrategies #-}

module Control.Monad.NoEarlyTermination
  ( NoEarlyTerminationT
  , runNoEarlyTerminationT
  ) where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.EarlyTerminate
import Control.Monad.Interleave (MonadInterleave)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Vector.Generic.Mutable qualified as VGM

newtype NoEarlyTerminationT m a = NoEarlyTerminationT (m a)
  deriving newtype (Functor, Applicative, Monad, MonadThrow, MonadInterleave, VGM.PrimMonad)

instance MonadTrans NoEarlyTerminationT where
  lift = NoEarlyTerminationT

deriving newtype instance MonadBase        IO m => MonadBase        IO (NoEarlyTerminationT m)
deriving newtype instance MonadBaseControl IO m => MonadBaseControl IO (NoEarlyTerminationT m)

runNoEarlyTerminationT :: NoEarlyTerminationT m a -> m a
runNoEarlyTerminationT (NoEarlyTerminationT x) = x

instance Monad m => MonadEarlyTerminate (NoEarlyTerminationT m) where
  earlyTerminationPoint = id
