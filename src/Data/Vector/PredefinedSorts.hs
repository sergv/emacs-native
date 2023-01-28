----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.PredefinedSorts
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-orphans #-}

{-# OPTIONS_GHC -O2 #-}

{-# OPTIONS_GHC -ddump-simpl -dsuppress-uniques -dsuppress-coercions -dppr-cols200 -ddump-to-file #-}

module Data.Vector.PredefinedSorts
  ( qsortInt32
  , qsortWord64
  , qsortWord64Par
  , qsortSortKeyPar
  , sortVectorUnsafeChar
  ) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Fork
import Control.Monad.ST
import Data.FuzzyMatch.SortKey
import Data.Int
import Data.Vector.Ext qualified as VExt
import Data.Vector.FixedSort qualified as FixedSort
import Data.Vector.Primitive qualified as P
import Data.Vector.Primitive.Mutable qualified as PM
import Data.Word

{-# SPECIALISE FixedSort.bitonicSort :: Int -> PM.MVector s Int32 -> ST s () #-}
{-# SPECIALISE VExt.qsort            :: Sequential -> PM.MVector s Int32 -> ST s () #-}
{-# SPECIALISE FixedSort.sort3       :: PM.MVector s Int32 -> ST s () #-}
{-# SPECIALISE FixedSort.sort4       :: PM.MVector s Int32 -> ST s () #-}

{-# SPECIALISE FixedSort.bitonicSort :: Int -> PM.MVector s Word64 -> ST s () #-}
{-# SPECIALISE VExt.qsort            :: Sequential -> PM.MVector s Word64 -> ST s () #-}
{-# SPECIALISE VExt.qsort            :: Parallel -> PM.MVector RealWorld Word64 -> IO () #-}
{-# SPECIALISE FixedSort.sort3       :: PM.MVector s Word64 -> ST s () #-}
{-# SPECIALISE FixedSort.sort4       :: PM.MVector s Word64 -> ST s () #-}

{-# SPECIALISE FixedSort.bitonicSort :: Int -> PM.MVector RealWorld SortKey -> IO () #-}
{-# SPECIALISE VExt.qsort            :: Parallel -> PM.MVector RealWorld SortKey -> IO () #-}
{-# SPECIALISE FixedSort.sort3       :: PM.MVector RealWorld SortKey -> IO () #-}
{-# SPECIALISE FixedSort.sort4       :: PM.MVector RealWorld SortKey -> IO () #-}

{-# SPECIALISE FixedSort.bitonicSort :: Int -> P.MVector s Char -> ST s () #-}
{-# SPECIALISE VExt.qsort            :: Sequential -> P.MVector s Char -> ST s () #-}
{-# SPECIALISE FixedSort.sort3       :: P.MVector s Char -> ST s () #-}
{-# SPECIALISE FixedSort.sort4       :: P.MVector s Char -> ST s () #-}

{-# NOINLINE qsortInt32 #-}
qsortInt32 :: PM.MVector s Int32 -> ST s ()
qsortInt32 = VExt.qsort Sequential

{-# NOINLINE qsortWord64 #-}
qsortWord64 :: PM.MVector s Word64 -> ST s ()
qsortWord64 = VExt.qsort Sequential

{-# NOINLINE qsortWord64Par #-}
qsortWord64Par :: PM.MVector RealWorld Word64 -> IO ()
qsortWord64Par xs = do
  p <- mkParallel =<< getNumCapabilities
  VExt.qsort p xs

{-# NOINLINE qsortSortKeyPar #-}
qsortSortKeyPar :: PM.MVector RealWorld SortKey -> IO ()
qsortSortKeyPar xs = do
  p <- mkParallel =<< getNumCapabilities
  VExt.qsort p xs

{-# NOINLINE sortVectorUnsafeChar #-}
sortVectorUnsafeChar :: P.Vector Char -> P.Vector Char
sortVectorUnsafeChar = VExt.sortVectorUnsafe
