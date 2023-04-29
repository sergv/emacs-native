----------------------------------------------------------------------------
-- |
-- Module      :  Data.Regex
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Regex
  ( fileGlobsToRegex
  , compileRe
  , compileReWithOpts
  , reMatches
  , reMatchesOsPath
  , reMatchesString
  , reMatchesByteString
  , reMatchesShortByteString
  , reAllByteStringMatches

    -- * Reexports
  , module Text.Regex.TDFA
  ) where

import Control.Monad.Catch (MonadThrow(..))
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as BSS
import Data.Foldable
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter
import System.OsPath
import Text.Regex.TDFA
import Text.Regex.TDFA.Text qualified as TDFA

import Data.Emacs.Path
import Emacs.Module.Assert (WithCallStack)
import Emacs.Module.Errors

fileGlobsToRegex
  :: (WithCallStack, MonadThrow m, Foldable f, Functor f)
  => f Text -> m Regex
fileGlobsToRegex
  = compileReWithOpts compOpts
  . mkStartEnd
  . mkGroup
  . T.intercalate "|"
  . toList
  . fmap (mkGroup . T.concatMap f)
  where
    mkGroup :: Text -> Text
    mkGroup = T.cons '(' . (`T.snoc` ')')
    mkStartEnd :: Text -> Text
    mkStartEnd = T.cons '^' . (`T.snoc` '$')
    f :: Char -> Text
    f '*'  = ".*"
    f '.'  = "\\."
    f '+'  = "\\+"
    f '['  = "\\["
    f ']'  = "\\]"
    f '('  = "\\("
    f ')'  = "\\)"
    f '^'  = "\\^"
    f '$'  = "\\$"
    f '?'  = "\\?"
#ifdef mingw32_HOST_OS
    f '\\' = "[\\/]"
    f '/'  = "[\\/]"
#else
    f '\\' = "\\\\"
#endif
    f c    = T.singleton c

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

reMatches :: Regex -> Text -> Bool
reMatches = match

reMatchesOsPath :: Regex -> OsPath -> Bool
reMatchesOsPath re = match re . pathToText

reMatchesString :: Regex -> String -> Bool
reMatchesString = match

reMatchesByteString :: Regex -> C8.ByteString -> Bool
reMatchesByteString = match

reMatchesShortByteString :: Regex -> ShortByteString -> Bool
reMatchesShortByteString re = match re . BSS.fromShort

reAllByteStringMatches
  :: Regex -> C8.ByteString -> AllMatches [] (MatchOffset, MatchLength)
reAllByteStringMatches = match
