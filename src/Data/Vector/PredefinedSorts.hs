----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.PredefinedSorts
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Vector.PredefinedSorts
  ( sortInt32
  , sortWord64
  , sortPackedCharAndIdx
  , sortVectorUnsafeChar
  , sortSortKeyPar
  ) where

import Control.Monad.ST
import Data.FuzzyMatch.SortKey
import Data.Int
import Data.Vector.Primitive qualified as P
import Data.Vector.Primitive.Mutable qualified as PM
import Data.Vector.Unboxed.Mutable qualified as UM
import Data.Word

import Data.Packed

import Data.Vector.Algorithms.FixedSort qualified as Quick
import Data.Vector.Algorithms.Heapsort qualified as Quick
import Data.Vector.Algorithms.Quicksort.Parameterised qualified as Quick

{-# SPECIALIZE Quick.sortInplaceFM :: Quick.Sequential -> Quick.Median3 Int32 -> PM.MVector s Int32 -> ST s () #-}
{-# SPECIALIZE Quick.heapSort      :: PM.MVector s Int32 -> ST s ()        #-}
{-# SPECIALIZE Quick.bitonicSort   :: Int -> PM.MVector s Int32 -> ST s () #-}

{-# SPECIALIZE Quick.sortInplaceFM :: Quick.Sequential -> Quick.Median3 Word64 -> PM.MVector s Word64 -> ST s () #-}
{-# SPECIALIZE Quick.heapSort      :: PM.MVector s Word64 -> ST s ()        #-}
{-# SPECIALIZE Quick.bitonicSort   :: Int -> PM.MVector s Word64 -> ST s () #-}

{-# SPECIALIZE Quick.sortInplaceFM :: Quick.Sequential -> Quick.Median3 CharAndIdxs -> UM.MVector s CharAndIdxs -> ST s () #-}
{-# SPECIALIZE Quick.heapSort      :: UM.MVector s CharAndIdxs -> ST s ()        #-}
{-# SPECIALIZE Quick.bitonicSort   :: Int -> UM.MVector s CharAndIdxs -> ST s () #-}

{-# SPECIALIZE Quick.sortInplaceFM :: Quick.Sequential -> Quick.Median3 Char -> PM.MVector s Char -> ST s () #-}
{-# SPECIALIZE Quick.heapSort      :: PM.MVector s Char  -> ST s ()                                          #-}
{-# SPECIALIZE Quick.bitonicSort   :: Int                -> PM.MVector s Char -> ST s ()                     #-}


{-# SPECIALIZE Quick.sortInplaceFM :: Quick.ParStrategies -> Quick.Median3or5 SortKey -> PM.MVector s SortKey -> ST s () #-}
{-# SPECIALIZE Quick.heapSort      :: PM.MVector s SortKey  -> ST s ()                                          #-}
{-# SPECIALIZE Quick.bitonicSort   :: Int                -> PM.MVector s SortKey -> ST s ()                     #-}


{-# NOINLINE sortInt32 #-}
sortInt32 :: PM.MVector s Int32 -> ST s ()
sortInt32 = Quick.sortInplaceFM Quick.Sequential (Quick.Median3 @Int32)

{-# NOINLINE sortWord64 #-}
sortWord64 :: PM.MVector s Word64 -> ST s ()
sortWord64 = Quick.sortInplaceFM Quick.Sequential (Quick.Median3 @Word64)

{-# NOINLINE sortPackedCharAndIdx #-}
sortPackedCharAndIdx :: UM.MVector s CharAndIdxs -> ST s ()
sortPackedCharAndIdx = Quick.sortInplaceFM Quick.Sequential (Quick.Median3 @CharAndIdxs)

sortChar :: PM.MVector s Char -> ST s ()
sortChar = Quick.sortInplaceFM Quick.Sequential (Quick.Median3 @Char)

{-# NOINLINE sortVectorUnsafeChar #-}
sortVectorUnsafeChar :: P.Vector Char -> P.Vector Char
sortVectorUnsafeChar !xs = runST $ do
  ys <- P.unsafeThaw xs
  sortChar ys
  P.unsafeFreeze ys

{-# NOINLINE sortSortKeyPar #-}
sortSortKeyPar :: PM.MVector s SortKey -> ST s ()
sortSortKeyPar =
  Quick.sortInplaceFM (Quick.setParStrategiesCutoff 1000 Quick.defaultParStrategies) (Quick.Median3or5 @SortKey)
