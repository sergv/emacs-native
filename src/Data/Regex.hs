----------------------------------------------------------------------------
-- |
-- Module      :  Data.Regex
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Regex
  ( globsToRegex
  , compileRe
  , compileReWithOpts
  , reMatches
  , reMatchesPath
  , reMatchesString
  , reAllByteStringMatches

    -- * Reexports
  , module Text.Regex.TDFA
  ) where

import Control.Exception.Safe.Checked (Throws, MonadThrow)
import qualified Control.Exception.Safe.Checked as Checked

import qualified Data.ByteString.Char8 as C8
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Path
import qualified System.FilePath
import Text.Regex.TDFA
import qualified Text.Regex.TDFA.Text as TDFA

import Emacs.Module.Assert (WithCallStack)
import Emacs.Module.Errors

globsToRegex
  :: (WithCallStack, Throws UserError, MonadThrow m, Foldable f, Functor f)
  => f Text -> m Regex
globsToRegex =
  compileReWithOpts compOpts . mkStartEnd . mkGroup . T.intercalate "|" . toList . fmap (mkGroup . T.concatMap f)
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
    f '\\' = "\\\\"
    f c    = T.singleton c

    compOpts = defaultCompOpt
      { multiline      = False
      , caseSensitive  = isLinux
      , lastStarGreedy = True
      }
    isLinux = System.FilePath.pathSeparator == '/'

compileRe :: (WithCallStack, MonadThrow m, Throws UserError) => Text -> m Regex
compileRe = compileReWithOpts compOpts
  where
    compOpts = defaultCompOpt
      { multiline     = False
      , caseSensitive = True
      }

compileReWithOpts
  :: (WithCallStack, MonadThrow m, Throws UserError)
  => CompOption -> Text -> m Regex
compileReWithOpts compOpts re =
  case TDFA.compile compOpts execOpts re of
    Left err -> Checked.throw $ mkUserError "compileRe" $
      "Failed to compile regular expression:" <+> pretty err <> ":" <> line <> pretty re
    Right x  -> pure x
  where
    execOpts = defaultExecOpt
      { captureGroups = False
      }

reMatches :: Regex -> Text -> Bool
reMatches = match

reMatchesPath :: Regex -> Path a b -> Bool
reMatchesPath re = match re . toFilePath

reMatchesString :: Regex -> String -> Bool
reMatchesString = match

reAllByteStringMatches
  :: Regex -> C8.ByteString -> AllMatches [] (MatchOffset, MatchLength)
reAllByteStringMatches = match
