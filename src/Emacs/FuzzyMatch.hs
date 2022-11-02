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

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Emacs.FuzzyMatch (initialise) where

import Control.Concurrent.Async.Lifted.Safe
import Control.Monad.IO.Class
import Control.Monad.Par
import Control.Monad.ST
import Control.Monad.Trans.Control
import Data.Coerce
import Data.Int
import Data.Ord
import Data.Primitive.PrimArray
import Data.Primitive.Sort
import Data.Primitive.Types
import Data.Text qualified as T

import Data.Emacs.Module.Doc qualified as Doc
import Emacs.Module
import Emacs.Module.Assert (WithCallStack)
import Emacs.Module.Monad qualified as Emacs

import Data.Vector qualified as V
import Data.Vector.Ext qualified as VExt
import Data.Vector.Growable qualified as VG

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
extractSeps xs = do
  ys <- traversePrimArray (fmap fromIntegral . extractInt) =<< extractVectorAsPrimArray xs
  pure $ runST $ unsafeFreezePrimArray =<< sortUniqueMutable =<< unsafeThawPrimArray ys

{-# INLINE extractListWithViaGrowableVector #-}
-- | Extract vanilla Emacs list as a Haskell list.
extractListWithViaGrowableVector
  :: (WithCallStack, MonadEmacs m v)
  => (v s -> m s a)
  -> v s
  -> m s (V.Vector a)
extractListWithViaGrowableVector f input = do
  ys <- VG.new 0
  VG.unsafeFreeze =<< extractLoop ys input
  where
    extractLoop ys xs = do
      nonNil <- isNotNil xs
      if nonNil
      then do
        x <- f =<< car xs
        ys' <- VG.push x ys
        extractLoop ys' =<< cdr xs
      else
        pure ys

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

newtype SortKey v = SortKey { _unSortKey :: (Int32, Int, v) }

instance Eq (SortKey v) where
  SortKey (a, b, _) == SortKey (a', b', _) = a == a' && b == b'

instance Ord (SortKey v) where
  SortKey (a, b, _) `compare` SortKey (a', b', _) = Down a `compare` Down a' <> b `compare` b'

scoreMatches
  :: forall m v s. (WithCallStack, MonadEmacs m v, MonadIO (m s), MonadThrow (m s), MonadBaseControl IO (m s), Forall (Pure (m s)), NFData (v s), Prim (v s))
  => EmacsFunction ('S ('S ('S 'Z))) 'Z 'False m v s
scoreMatches (R seps (R needle (R haystacks Stop))) = do
  seps'      <- extractSeps seps
  needle'    <- extractText needle
  -- haystacks' <- extractListWith' (\str -> (, str) <$> extractText str) haystacks
  haystacks' <- extractListWithViaGrowableVector (\str -> (, str) <$> extractText str) haystacks
  -- let matches = snd <$> haystacks'

  let needleChars = prepareNeedle needle'
      matches
        = fmap (\(SortKey (_, _, emacsStr)) -> emacsStr)
        $ VExt.sortVectorUnsafe
        $ (\xs -> coerce xs :: V.Vector (SortKey (v s)))
        -- $ L.sortOn (\(score, len, _emacsStr) -> (Down score, len))
        -- $ runPar
        -- $ parMap (\(str, emacsStr) -> (mScore $ fuzzyMatch (computeHeatMap str seps') needle' str, str, emacsStr)) haystacks'
        $ fmap
            (\(haystack, emacsStr) ->
              ( fi32 $ mScore $ fuzzyMatch (computeHeatMap haystack seps') needle' needleChars haystack
              , T.length haystack
              , emacsStr
              ))
            haystacks'
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
  let Match{mScore, mPositions} = fuzzyMatch (computeHeatMap haystack' seps') needle' (prepareNeedle needle') haystack'
  score     <- makeInt $ fromIntegral mScore
  positions <- makeList =<< traverse (makeInt . fromIntegral . unStrIdx) mPositions
  cons score positions
