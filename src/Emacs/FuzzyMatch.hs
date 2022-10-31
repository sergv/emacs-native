----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.FuzzyMatch
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Emacs.FuzzyMatch (initialise) where

import Control.Concurrent.Async.Lifted.Safe
import Control.Monad.IO.Class
import Control.Monad.Par
import Control.Monad.ST
import Control.Monad.Trans.Control
import Data.List qualified as L
import Data.Ord
import Data.Primitive.PrimArray
import Data.Primitive.Sort
import Data.Primitive.Types
import Data.Text qualified as T

import Data.Emacs.Module.Doc qualified as Doc
import Emacs.Module
import Emacs.Module.Assert (WithCallStack)
import Emacs.Module.Monad qualified as Emacs

import Data.FuzzyMatch

initialise
  :: WithCallStack
  => Emacs.EmacsM s ()
initialise = do
  bindFunction "haskell-native-score-matches" =<<
    makeFunction scoreMatches scoreMatchesDoc
  bindFunction "haskell-native-score-single-match" =<<
    makeFunction scoreSingleMatch scoreSingleMatchDoc

scoreMatchesDoc :: Doc.Doc
scoreMatchesDoc =
  "Given a query string and a list of strings to match against, \
  \sort the strings according to score of fuzzy matching them against the query."

extractSeps :: (MonadEmacs m v, Prim (v s)) => v s -> m s (PrimArray Int)
extractSeps xs = do
  ys <- traversePrimArray extractInt =<< extractVectorAsPrimArray xs
  pure $ runST $ unsafeFreezePrimArray =<< sortUniqueMutable =<< unsafeThawPrimArray ys

scoreMatches
  :: forall m v s. (WithCallStack, MonadEmacs m v, MonadIO (m s), MonadThrow (m s), MonadBaseControl IO (m s), Forall (Pure (m s)), NFData (v s), Prim (v s))
  => EmacsFunction ('S ('S ('S 'Z))) 'Z 'False m v s
scoreMatches (R seps (R needle (R haystacks Stop))) = do
  seps'      <- extractSeps seps
  needle'    <- extractText needle
  haystacks' <- extractListWith (\str -> (, str) <$> extractText str) haystacks
  let matches
        = map (\(_, _, emacsStr) -> emacsStr)
        $ L.sortOn (\(score, str, _emacsStr) -> (Down score, T.length str))
        $ runPar
        $ parMap (\(str, emacsStr) -> (mScore $ fuzzyMatch (computeHeatMap str seps') needle' str, str, emacsStr)) haystacks'
  makeList matches

scoreSingleMatchDoc :: Doc.Doc
scoreSingleMatchDoc =
  "Fuzzy match a single string against another. Returns match score and \
  \positions where the match occured."

scoreSingleMatch
  :: forall m v s. (WithCallStack, MonadEmacs m v, MonadIO (m s), MonadThrow (m s), MonadBaseControl IO (m s), Forall (Pure (m s)), Prim (v s))
  => EmacsFunction ('S ('S ('S 'Z))) 'Z 'False m v s
scoreSingleMatch (R seps (R needle (R haystack Stop))) = do
  seps'     <- extractSeps seps
  needle'   <- extractText needle
  haystack' <- extractText haystack
  let Match{mScore, mPositions} = fuzzyMatch (computeHeatMap haystack' seps') needle' haystack'
  score     <- makeInt mScore
  positions <- makeList =<< traverse (makeInt . fromIntegral . unStrIdx) mPositions
  cons score positions
