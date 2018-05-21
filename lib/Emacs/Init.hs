----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Init
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  10 May 2018
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module Emacs.Init (initialise) where

import Control.Monad.IO.Class

import Foreign
import Foreign.C
import GHC.Conc

import Data.Emacs.Module.Runtime (Runtime)
import qualified Data.Emacs.Module.Runtime as Runtime
import Data.Emacs.Module.SymbolName.TH
import Emacs.Module
import Emacs.Module.Assert

import Emacs.FastFileSearch

foreign export ccall initialise :: Ptr Runtime -> IO CBool

true, false :: CBool
true  = CBool 1
false = CBool 0

initialise :: WithCallStack => Ptr Runtime -> IO CBool
initialise runtime = do
  runtime' <- Runtime.validateRuntime runtime
  case runtime' of
    Nothing        -> pure false
    Just runtime'' -> do
      env <- Runtime.getEnvironment runtime''
      res <- reportAllErrorsToEmacs env (pure False) $ runEmacsM env initialise'
      pure $ if res then true else false

initialise' :: (WithCallStack, MonadEmacs m, MonadIO m) => m Bool
initialise' = do
  liftIO $ setNumCapabilities 4
  emacsFindFun <- makeFunction emacsFindRec emacsFindRecDoc
  bindFunction [esym|haskell-native-find-rec|] emacsFindFun
  _ <- provide [esym|haskell-native-emacs-extensions|]
  pure True
