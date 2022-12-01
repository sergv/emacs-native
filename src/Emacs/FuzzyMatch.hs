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
import Data.Vector.PredefinedSorts

import Data.Emacs.Module.Doc qualified as Doc
import Emacs.Module
import Emacs.Module.Assert (WithCallStack)
import Emacs.Module.Monad qualified as Emacs

-- import Data.Primitive.Array.Growable qualified as AG
-- import Data.Primitive.PrimArray.Growable qualified as PG
-- import Data.Primitive.SmallArray.Growable qualified as SAG
import Data.Vector qualified as V
-- import Data.Vector.Growable qualified as VG

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
  ys <- traversePrimArray (fmap fromIntegral . extractInt) =<< extractVectorAsPrimArray xs
  pure $ runST $ unsafeFreezePrimArray =<< sortUniqueMutable =<< unsafeThawPrimArray ys

-- {-# INLINE extractListWithViaGrowableVector #-}
-- -- | Extract vanilla Emacs list as a Haskell list.
-- extractListWithViaGrowableVector
--   :: (WithCallStack, MonadEmacs m v)
--   => (v s -> m s a)
--   -> v s
--   -> m s (V.Vector a)
-- extractListWithViaGrowableVector f !input = do
--   !ys <- VG.new 0
--   VG.unsafeFreeze =<< extractLoop ys input
--   where
--     extractLoop !ys !xs = do
--       !nonNil <- isNotNil xs
--       if nonNil
--       then do
--         !x   <- f =<< car xs
--         !ys' <- VG.push x ys
--         extractLoop ys' =<< cdr xs
--       else
--         pure ys
--
-- {-# INLINE extractListWithViaGrowableArray #-}
-- -- | Extract vanilla Emacs list as a Haskell list.
-- extractListWithViaGrowableArray
--   :: (WithCallStack, MonadEmacs m v)
--   => (v s -> m s a)
--   -> v s
--   -> m s (V.Vector a)
-- extractListWithViaGrowableArray f !input = do
--   !ys <- AG.new 0
--   AG.unsafeToVector =<< extractLoop ys input
--   where
--     extractLoop !ys !xs = do
--       !nonNil <- isNotNil xs
--       if nonNil
--       then do
--         !x   <- f =<< car xs
--         !ys' <- AG.push x ys
--         extractLoop ys' =<< cdr xs
--       else
--         pure ys
--
-- {-# INLINE extractListWithViaGrowableSmallArray #-}
-- -- | Extract vanilla Emacs list as a Haskell list.
-- extractListWithViaGrowableSmallArray
--   :: (WithCallStack, MonadEmacs m v)
--   => (v s -> m s a)
--   -> v s
--   -> m s (SAG.SmallArray a)
-- extractListWithViaGrowableSmallArray f !input = do
--   !ys <- SAG.new 0
--   SAG.unsafeFreeze =<< extractLoop ys input
--   where
--     extractLoop !ys !xs = do
--       !nonNil <- isNotNil xs
--       if nonNil
--       then do
--         !x   <- f =<< car xs
--         !ys' <- SAG.push x ys
--         extractLoop ys' =<< cdr xs
--       else
--         pure ys

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

-- {-# NOINLINE scoreMatches #-}
scoreMatches
  :: forall m v s. (WithCallStack, MonadEmacs m v, MonadIO (m s), MonadThrow (m s), MonadBaseControl IO (m s), Forall (Pure (m s)), NFData (v s), Prim (v s))
  => EmacsFunction ('S ('S ('S 'Z))) 'Z 'False m v s
scoreMatches (R seps (R needle (R haystacks Stop))) = {-# SCC "scoreMatches" #-} do
  seps'      <- extractSeps seps
  needle'    <- extractText needle
  -- haystacks' <- extractListWith' (\str -> (, str) <$> extractText str) haystacks

  -- (haystacks' :: V.Vector (Text, v s)) <- traverse (\str -> (, str) <$> extractText str) =<< extractVector haystacks
  (haystacks' :: V.Vector (Text, v s)) <- extractVectorWith (\str -> (, str) <$> extractText str) haystacks
  -- haystacks' <- extractListWithViaGrowableVector (\str -> (, str) <$> extractText str) haystacks
  -- haystacks' <- extractListWithViaGrowableArray (\str -> (, str) <$> extractText str) haystacks
  -- haystacks' <- extractListWithViaGrowableSmallArray (\str -> (, str) <$> extractText str) haystacks

  -- let matches = snd <$> haystacks'

  let needleChars = prepareNeedle needle'

  -- let matches = snd <$> haystacks'

  let haystacks'' = runST $ do
        store <- mkReusableState (T.length needle') needleChars

        for haystacks' $ \(haystack, emacsStr) -> do
          !match <- fuzzyMatch store (computeHeatmap haystack seps') needle' needleChars haystack
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
        fuzzyMatch store (computeHeatmap haystack' seps') needle' needleChars haystack'
  score     <- makeInt $ fromIntegral mScore
  positions <- makeList =<< traverse (makeInt . fromIntegral . unStrIdx) mPositions
  cons score positions
