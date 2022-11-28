----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Init
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  10 May 2018
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE ImportQualifiedPost      #-}
{-# LANGUAGE OverloadedStrings        #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Emacs.Init (initialise) where

import Foreign
import Foreign.C

import Data.Emacs.Module.Runtime (Runtime)
import Data.Emacs.Module.Runtime qualified as Runtime
import Data.Emacs.Module.SymbolName
import Emacs.Module
import Emacs.Module.Assert

import Emacs.EprojTagIndex qualified
import Emacs.FastFileSearch qualified
import Emacs.FuzzyMatch qualified
import Emacs.Grep qualified
import Emacs.Module.Monad qualified as Emacs

foreign export ccall initialise :: Ptr Runtime -> IO CBool

true, false :: CBool
true  = CBool 1
false = CBool 0

initialise :: WithCallStack => Ptr Runtime -> IO CBool
initialise runtime = do
  runtime' <- Runtime.validateRuntime runtime
  case runtime' of
    Nothing        -> pure false
    Just runtime'' ->
      Runtime.withEnvironment runtime'' $ \env -> do
        res <- reportAllErrorsToEmacs env (pure False) $ Emacs.runEmacsM env initialise'
        pure $ if res then true else false

initialise'
  :: WithCallStack
  => Emacs.EmacsM s Bool
initialise' = do
  Emacs.FastFileSearch.initialise
  Emacs.FuzzyMatch.initialise
  Emacs.Grep.initialise
  Emacs.EprojTagIndex.initialise
  _ <- provide $ mkSymbolName "haskell-native-emacs-extensions"
  pure True
