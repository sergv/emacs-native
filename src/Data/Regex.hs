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
  , compileReWithOpts
  , reMatches
  , reMatchesOsPath
  , reMatchesShortByteString
  , reAllByteStringMatches

  , compileReWithOptsUnicodeAsBytes

  -- * Reexports
  , Text.Regex.TDFA.Regex
  , Text.Regex.TDFA.RegexOptions(..)
  , Text.Regex.TDFA.CompOption(..)
  , Text.Regex.TDFA.AllMatches(..)
  , Text.Regex.TDFA.MatchOffset
  , Text.Regex.TDFA.MatchLength
  ) where

import Control.Monad.Catch (MonadThrow(..))
import Data.ByteString.Lazy.Char8 qualified as CL8
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as BSS
import Data.Coerce
import Data.Foldable
import Data.Text (Text)
import Data.Text.Builder.Linear.Buffer
import Data.Text.Encoding qualified as T
import Data.Text.Ext (textFoldLinear)
import Prettyprinter
import System.OsPath
import System.OsPath.Ext
import Text.Regex.TDFA
import Text.Regex.TDFA.Text qualified as TDFA

import Emacs.Module.Assert (WithCallStack)
import Emacs.Module.Errors

fileGlobsToRegex
  :: (WithCallStack, MonadThrow m, Foldable f, Functor f, Coercible a Text)
  => f a -> m Regex
fileGlobsToRegex patterns = compileReWithOpts compOpts $ runBuffer initRe
  where
    initRe :: Buffer %1 -> Buffer
    initRe buf =
      mkRe (coerce (toList patterns)) (buf |>. '^' |>. '(') |>. ')' |>. '$'

    mkRe :: [Text] -> Buffer %1 -> Buffer
    mkRe []       buf = buf
    mkRe (x : xs) buf =
      -- mkRe' xs ((textFoldLinear f (buf |>. '(') x) |>. ')')
      mkRe' xs (textFoldLinear f buf x)

    mkRe' :: [Text] -> Buffer %1 -> Buffer
    mkRe' []       buf = buf
    mkRe' (x : xs) buf =
      -- mkRe' xs ((textFoldLinear f (buf |>. '|' |>. '(') x) |>. ')')
      mkRe' xs (textFoldLinear f (buf |>. '|') x)

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

    compOpts = defaultCompOpt
      { multiline      = False
      , caseSensitive  = isLinux
      , lastStarGreedy = True
      }
    isLinux =
#ifdef mingw32_HOST_OS
      False
#else
      True
#endif

compileRe :: (WithCallStack, MonadThrow m) => Text -> m Regex
compileRe = compileReWithOpts compOpts
  where
    compOpts = defaultCompOpt
      { multiline     = False
      , caseSensitive = True
      }

compileReWithOpts
  :: (WithCallStack, MonadThrow m)
  => CompOption -> Text -> m Regex
compileReWithOpts compOpts re =
  case TDFA.compile compOpts execOpts re of
    Left err -> throwM $ mkUserError "compileRe" $
      "Failed to compile regular expression:" <+> pretty err <> ":" <> line <> pretty re
    Right x  -> pure x
  where
    execOpts = defaultExecOpt
      { captureGroups = False
      }

compileReWithOptsUnicodeAsBytes
  :: (WithCallStack, MonadThrow m)
  => CompOption -> Text -> m Regex
compileReWithOptsUnicodeAsBytes compOpts re =
  case TDFA.compile compOpts execOpts $ T.decodeLatin1 $ T.encodeUtf8 re of
    Left err -> throwM $ mkUserError "compileRe" $
      "Failed to compile regular expression:" <+> pretty err <> ":" <> line <> pretty re
    Right x  -> pure x
  where
    execOpts = defaultExecOpt
      { captureGroups = False
      }

reMatches :: Regex -> Text -> Bool
reMatches = match

reMatchesOsPath :: Regex -> OsPath -> Bool
reMatchesOsPath re = match re . pathToText

reMatchesShortByteString :: Regex -> ShortByteString -> Bool
reMatchesShortByteString re = match re . BSS.fromShort

reAllByteStringMatches
  :: Regex -> CL8.ByteString -> AllMatches [] (MatchOffset, MatchLength)
reAllByteStringMatches = match
