-- |
-- Module:     Control.Monad.EarlyTerminate
-- Copyright:  (c) Sergey Vinokurov 2026
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.EarlyTerminate
  ( MonadEarlyTerminate(..)
  , EarlyTermination(..)
  ) where

import Control.Monad.Catch
import Data.Emacs.Module.Env.ProcessInput qualified as ProcessInput
import Emacs.Module.Monad qualified as Emacs
import Prettyprinter (Pretty(..))

-- | User requested to terminate computation early via C-g.
data EarlyTermination = EarlyTermination
  deriving (Show)

instance Exception EarlyTermination

instance Pretty EarlyTermination where
  pretty EarlyTermination = "EarlyTermination"

class Monad m => MonadEarlyTerminate m where
  -- Terminate somehow if some condition holds by e.g. throwing
  -- exception or keep executing passed action of condition doesn’t
  -- hold.
  earlyTerminationPoint :: m a -> m a

instance MonadEarlyTerminate (Emacs.EmacsM s) where
  earlyTerminationPoint cont =
    Emacs.processInput >>= \case
      ProcessInput.Quit     -> throwM EarlyTermination
      ProcessInput.Continue -> cont
