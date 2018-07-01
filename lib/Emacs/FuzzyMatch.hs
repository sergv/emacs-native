----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.FuzzyMatch
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Emacs.FuzzyMatch (initialise) where

import Control.Concurrent.Async.Lifted.Safe
import Control.Monad.IO.Class
import Control.Monad.Par
import Control.Monad.Trans.Control

import qualified Data.ByteString.Char8 as C8
import Data.Foldable
import qualified Data.List as L
import Data.Ord
import qualified Data.Text as T

import Data.Emacs.Module.SymbolName.TH
import Emacs.Module
import Emacs.Module.Assert (WithCallStack)
import Emacs.Module.Errors

import Data.FuzzyMatch

initialise
  :: (WithCallStack, Throws EmacsThrow, Throws EmacsError, Throws EmacsInternalError)
  => EmacsM s ()
initialise = do
  bindFunction [esym|haskell-native-score-matches|] =<<
    makeFunction scoreMatches scoreMatchesDoc
  bindFunction [esym|haskell-native-score-single-match|] =<<
    makeFunction scoreSingleMatch scoreSingleMatchDoc

scoreMatchesDoc :: C8.ByteString
scoreMatchesDoc =
  "Given a query string and a list of strings to match against, \
  \sort the strings according to score of fuzzy matching them against the query."

scoreMatches
  :: forall m s. (WithCallStack, MonadEmacs m, Monad (m s), MonadIO (m s), MonadThrow (m s), MonadBaseControl IO (m s), Forall (Pure (m s)))
  => EmacsFunction ('S ('S 'Z)) 'Z 'False s m
scoreMatches (R needle (R haystacks Stop)) = do
  needle'    <- extractText needle
  haystacks' <- traverse (\str -> (, str) <$> extractText str) =<< extractListRev haystacks
  let matches
        = map (\(_, _, emacsStr) -> emacsStr)
        $ L.sortOn (\(score, str, _emacsStr) -> (Down score, T.length str))
        $ runPar
        $ parMap (\(str, emacsStr) -> (mScore $ fuzzyMatch (computeHeatMap str mempty) needle' str, str, emacsStr)) haystacks'
  makeList matches

scoreSingleMatchDoc :: C8.ByteString
scoreSingleMatchDoc =
  "."

scoreSingleMatch
  :: forall m s. (WithCallStack, MonadEmacs m, Monad (m s), MonadIO (m s), MonadThrow (m s), MonadBaseControl IO (m s), Forall (Pure (m s)))
  => EmacsFunction ('S ('S 'Z)) 'Z 'False s m
scoreSingleMatch (R needle (R haystack Stop)) = do
  needle'   <- extractText needle
  haystack' <- extractText haystack
  let Match{mScore, mPositions} = fuzzyMatch (computeHeatMap haystack' mempty) needle' haystack'
  score     <- makeInt mScore
  positions <- makeList =<< traverse (makeInt . unStrIdx) (toList mPositions)
  cons score positions
