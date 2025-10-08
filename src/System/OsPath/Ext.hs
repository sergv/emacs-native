-- |
-- Module:     System.OsPath.Ext
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wno-deprecations #-}

module System.OsPath.Ext
  ( textToShortByteString
  , textFromShortByteString
  , stripProperPrefix
  , pathToText
  , pathFromText
  , pathToByteString

  , pathToUtf8
  , pathFromUtf8
  ) where

import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as BSS
import Data.Coerce
import Data.Text (Text)
import Data.Text.Array qualified as TA
import Data.Text.Encoding qualified as TE
import Data.Text.Internal qualified as T
import System.OsPath
import System.OsString.Internal.Types

pathToUtf8 :: OsPath -> ShortByteString
pathToUtf8 =
#ifdef mingw32_HOST_OS
  _reconvertUtf16LEToUtf8 . getWindowsString . getOsString
#else
  getPosixString . getOsString
#endif

pathFromUtf8 :: ShortByteString -> OsPath
pathFromUtf8 =
#ifdef mingw32_HOST_OS
  OsString . WindowsString . _reconvertUtf8LEToUtf16
#else
  OsString . PosixString
#endif

pathToByteString :: OsPath -> C8.ByteString
pathToByteString = BSS.fromShort . pathToUtf8

pathToText :: OsPath -> Text
pathToText = textFromShortByteString . pathToUtf8

pathFromText :: Text -> OsPath
pathFromText = pathFromUtf8 . textToShortByteString

_reconvertUtf16LEToUtf8 :: ShortByteString -> ShortByteString
_reconvertUtf16LEToUtf8 = textToShortByteString . TE.decodeUtf16LE . BSS.fromShort

_reconvertUtf8LEToUtf16 :: ShortByteString -> ShortByteString
_reconvertUtf8LEToUtf16 = BSS.toShort . TE.encodeUtf16LE . textFromShortByteString

textToShortByteString :: Text -> ShortByteString
textToShortByteString (T.Text arr offset len) = BSS.SBS arr'
  where
    !(TA.ByteArray arr') = TA.run $ do
      dest <- TA.new len
      TA.copyI len dest 0 arr offset
      pure dest

textFromShortByteString :: ShortByteString -> Text
textFromShortByteString str@(BSS.SBS arr) = T.Text (TA.ByteArray arr) 0 (BSS.length str)

stripProperPrefix :: OsPath -> OsPath -> Maybe OsPath
stripProperPrefix = coerce BSS.stripPrefix
