----------------------------------------------------------------------------
-- |
-- Module      :  Data.Regex
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE CPP               #-}
{-# LANGUAGE LinearTypes       #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Regex
  ( fileGlobsToRegex

  , compileRe
  , compileReSet
  , compileReWithOpts
  , compileReSetWithOpts
  , reMatches
  , reMatchesOsPath
  , reSetMatchesOsPath
  , reMatchesShortByteString
  , reAllByteStringMatches
  , reAllUtf8PtrMatches

  -- * Reexports
  , Regex
  , RegexSet
  , Match(..)
  , ReversedList(..)

  , Flags
  , flagCaseInsensitive
  , flagMultiline
  , flagDotNewline
  , flagSwapGreed
  , flagIgnoreWhitespace
  , flagUnicode
  , flagDefault
  ) where

import Control.Monad.Catch (MonadThrow(..))
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as BSS
import Data.Coerce
import Data.Foldable
import Data.Regex.Rure
import Data.Text (Text)
import Data.Text.Builder.Linear.Buffer
import Data.Text.Encoding qualified as T
import Data.Text.Ext (textFoldLinear)
import Foreign.Ptr (Ptr, castPtr)
import Prettyprinter
import System.OsPath
import System.OsPath.Ext

import Emacs.Module.Assert (WithCallStack)
import Emacs.Module.Errors

fileGlobsToRegex
  :: (WithCallStack, MonadThrow m, Foldable f, Functor f, Coercible a Text)
  => f a -> m RegexSet
fileGlobsToRegex patterns =
  case compileRegexSet patterns' flags Nothing of
    Left err -> throwM $ mkUserError "fileGlobsToRegex" $
      "Failed to compile globs:" <+> pretty (T.decodeUtf8 err) <> ". Regexps:" <> line <> pretty (map T.decodeUtf8 patterns')
    Right x  -> pure x
  where
    patterns' :: [C8.ByteString]
    patterns' = map compileGlob (coerce (toList patterns))

    compileGlob :: Text -> C8.ByteString
    compileGlob pat =
      T.encodeUtf8 $
        runBuffer (\buf -> (textFoldLinear f (buf |>. '^' |>. '(') pat) |>. ')' |>. '$')

    f :: Char -> Buffer %1 -> Buffer
    f c buf = case c of
      '*'   -> buf |> ".*"
      '.'   -> buf |> "\\."
      '|'   -> buf |> "\\|"
      '+'   -> buf |> "\\+"
      '['   -> buf |> "\\["
      ']'   -> buf |> "\\]"
      '('   -> buf |> "\\("
      ')'   -> buf |> "\\)"
      '^'   -> buf |> "\\^"
      '$'   -> buf |> "\\$"
      '?'   -> buf |> "\\?"
#ifdef mingw32_HOST_OS
      '\\'  -> buf |> "[\\/]"
      '/'   -> buf |> "[\\/]"
#else
      '\\'  -> buf |> "\\\\"
#endif
      other -> buf |>. other

    flags = if isLinux then flagCaseInsensitive else mempty

    isLinux =
#ifdef mingw32_HOST_OS
      False
#else
      True
#endif

compileRe :: (WithCallStack, MonadThrow m) => C8.ByteString -> m Regex
compileRe = compileReWithOpts mempty

compileReSet :: (WithCallStack, MonadThrow m) => [C8.ByteString] -> m RegexSet
compileReSet = compileReSetWithOpts mempty

compileReWithOpts
  :: (WithCallStack, MonadThrow m)
  => Flags -> C8.ByteString -> m Regex
compileReWithOpts flags re =
  case compileRegex re flags Nothing of
    Left err -> throwM $ mkUserError "compileRe" $
      "Failed to compile regular expression:" <+> pretty (T.decodeUtf8 err) <> ":" <> line <> pretty (T.decodeUtf8 re)
    Right x  -> pure x

compileReSetWithOpts
  :: (WithCallStack, MonadThrow m)
  => Flags -> [C8.ByteString] -> m RegexSet
compileReSetWithOpts flags res =
  case compileRegexSet res flags Nothing of
    Left err -> throwM $ mkUserError "compileRe" $
      "Failed to compile regular expression:" <+> pretty (T.decodeUtf8 err) <> ":" <> line <> pretty (map T.decodeUtf8 res)
    Right x  -> pure x

reMatches :: Regex -> C8.ByteString -> Bool
reMatches = bytestringHasMatch

reMatchesOsPath :: Regex -> OsPath -> Bool
reMatchesOsPath re = reMatches re . pathToByteString

reSetMatchesOsPath :: RegexSet -> OsPath -> Bool
reSetMatchesOsPath reSet = bytestringHasSetMatch reSet . pathToByteString

reMatchesShortByteString :: Regex -> ShortByteString -> Bool
reMatchesShortByteString re = reMatches re . BSS.fromShort

reAllByteStringMatches
  :: Regex -> C8.ByteString -> ReversedList Match
reAllByteStringMatches = bytestringAllMatches

reAllUtf8PtrMatches
  :: Regex -> Ptr () -> Int -> IO (ReversedList Match)
reAllUtf8PtrMatches re ptr size =
  utf8PtrAllMatchesIO re (castPtr ptr) (fromIntegral size)

