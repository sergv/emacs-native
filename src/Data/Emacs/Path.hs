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
  , textToShortByteString
  , textFromShortByteString
  , stripProperPrefix
  , pathToText
  ) where

import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as BSS
import Data.Char
import Data.Coerce
import Data.Text (Text)
import Data.Text.Array qualified as TA
import Data.Text.Encoding qualified as TE
import Data.Text.Internal qualified as T
import System.OsPath
import System.OsString.Internal.Types

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

pathToUtf8 :: OsPath -> ShortByteString
pathToUtf8 =
#ifdef mingw32_HOST_OS
  _reconvertUtf16LEToUtf8 . getWindowsString . getOsString
#else
  getPosixString . getOsString
#endif

pathToText :: OsPath -> Text
pathToText = textFromShortByteString . pathToUtf8

_reconvertUtf16LEToUtf8 :: ShortByteString -> ShortByteString
_reconvertUtf16LEToUtf8 = textToShortByteString . TE.decodeUtf16LE . BSS.fromShort

textToShortByteString :: Text -> ShortByteString
textToShortByteString (T.Text (TA.ByteArray arr) _offset _len) = BSS.SBS arr

textFromShortByteString :: ShortByteString -> Text
textFromShortByteString str@(BSS.SBS arr) = T.Text (TA.ByteArray arr) 0 (BSS.length str)

stripProperPrefix :: OsPath -> OsPath -> Maybe OsPath
stripProperPrefix = coerce BSS.stripPrefix
