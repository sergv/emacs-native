----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Path
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

module Data.Emacs.Path
  ( pathForEmacs
  ) where

import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as BSS
import Data.Char
import System.OsPath
import System.OsPath.Ext

pathForEmacs :: OsPath -> ShortByteString
pathForEmacs = normaliseSeparators . pathToUtf8
  where
    normaliseSeparators =
#ifdef mingw32_HOST_OS
      _normaliseSeparatorsWindows
#else
      id
#endif
    _normaliseSeparatorsWindows =
      BSS.map (\c -> if c == fromIntegral (ord '\\') then fromIntegral (ord '/') else c)
