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

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE OverloadedStrings        #-}

module Emacs.Init (initialise) where

import Foreign
import Foreign.C

import Data.Emacs.Module.Runtime (Runtime)
import qualified Data.Emacs.Module.Runtime as Runtime
import Data.Emacs.Module.SymbolName
import Emacs.Module
import Emacs.Module.Assert
import Emacs.Module.Errors

import qualified Emacs.EprojTagIndex
import qualified Emacs.FastFileSearch
import qualified Emacs.FuzzyMatch
import qualified Emacs.Grep

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

initialise'
  :: (WithCallStack, Throws EmacsThrow, Throws EmacsError, Throws EmacsInternalError)
  => EmacsM s Bool
initialise' = do
  Emacs.FastFileSearch.initialise
  Emacs.FuzzyMatch.initialise
  Emacs.Grep.initialise
  Emacs.EprojTagIndex.initialise
  _ <- provide $ mkSymbolName "haskell-native-emacs-extensions"
  pure True
