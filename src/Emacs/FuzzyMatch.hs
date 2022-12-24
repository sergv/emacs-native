----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.FuzzyMatch
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Emacs.FuzzyMatch (initialise) where

import Control.Concurrent.Async.Lifted.Safe
import Control.Monad.IO.Class
import Control.Monad.Par
import Control.Monad.ST.Strict
import Control.Monad.Trans.Control
import Data.Coerce
import Data.Int
import Data.Primitive.PrimArray
import Data.Primitive.Sort
import Data.Primitive.Types
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable
import Data.Vector qualified as V
import Data.Vector.PredefinedSorts

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

extractSeps :: (MonadEmacs m v, Prim (v s)) => v s -> m s (PrimArray Int32)
extractSeps !xs = do
  ys <- extractVectorAsPrimArrayWith (fmap fromIntegral . extractInt) xs
  pure $ runST $ unsafeFreezePrimArray =<< sortUniqueMutable =<< unsafeThawPrimArray ys

{-# INLINE makeListFromVector #-}
-- | Construct vanilla Emacs list from a Haskell list.
makeListFromVector
  :: (WithCallStack, MonadEmacs m v)
  => V.Vector (v s)
  -> m s (v s)
makeListFromVector xs = do
  nilVal <- nil
  mkListLoop (V.length xs) nilVal
  where
    mkListLoop 0 res = pure res
    mkListLoop i res = do
      let !j = i - 1
      mkListLoop j =<< cons (xs `V.unsafeIndex` j) res

scoreMatches
  :: forall m v s. (WithCallStack, MonadEmacs m v, MonadIO (m s), MonadThrow (m s), MonadBaseControl IO (m s), Forall (Pure (m s)), NFData (v s), Prim (v s))
  => EmacsFunction ('S ('S ('S 'Z))) 'Z 'False m v s
scoreMatches (R seps (R needle (R haystacks Stop))) = {-# SCC "scoreMatches" #-} do
  seps'      <- extractSeps seps
  needle'    <- extractText needle

  (haystacks' :: V.Vector (Text, v s)) <- extractVectorWith (\str -> (, str) <$> extractText str) haystacks

  let needleChars = prepareNeedle needle'

  let haystacks'' = runST $ do
        store <- mkReusableState (T.length needle') needleChars

        for haystacks' $ \(haystack, emacsStr) -> do
          !match <- fuzzyMatch' store (computeHeatmap store haystack seps') needle' needleChars haystack
          pure (fi32 $ mScore $ match, T.length haystack, emacsStr)

  let matches
        = fmap (\(SortKey (_, _, emacsStr)) -> emacsStr)
        $ sortVectorUnsafeSortKey
        $ (\xs -> coerce xs :: V.Vector (SortKey (v s)))
        -- $ L.sortOn (\(score, len, _emacsStr) -> (Down score, len))
        -- $ runPar
        -- $ parMap (\(str, emacsStr) -> (mScore $ fuzzyMatch (computeHeatmap str seps') needle' str, str, emacsStr)) haystacks'
        $ haystacks''
  makeListFromVector matches

{-# INLINE fi32 #-}
fi32 :: Integral a => a -> Int32
fi32 = fromIntegral

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
  let needleChars = (prepareNeedle needle')
      !Match{mScore, mPositions} = runST $ do
        store <- mkReusableState (T.length needle') needleChars
        fuzzyMatch' store (computeHeatmap store haystack' seps') needle' needleChars haystack'
  score     <- makeInt $ fromIntegral mScore
  positions <- makeList =<< traverse (makeInt . fromIntegral . unStrIdx) mPositions
  cons score positions
