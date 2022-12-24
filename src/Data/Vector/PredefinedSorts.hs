----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.PredefinedSorts
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE ImportQualifiedPost #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-# OPTIONS_GHC -O2 #-}

{-# OPTIONS_GHC -ddump-simpl -dsuppress-uniques -dsuppress-coercions -dppr-cols200 -ddump-to-file #-}

module Data.Vector.PredefinedSorts
  ( qsortWord64
  , qsortSortKey
  , sortVectorUnsafeChar
  ) where

import Control.Monad.ST
import Data.FuzzyMatch.SortKey
import Data.Vector.Ext qualified as VExt
import Data.Vector.FixedSort qualified as FixedSort
import Data.Vector.Primitive qualified as P
import Data.Vector.Primitive.Mutable qualified as PM
import Data.Word

{-# SPECIALISE FixedSort.bitonicSort :: Int -> PM.MVector s Word64 -> ST s () #-}
{-# SPECIALISE VExt.qsort            :: PM.MVector s Word64 -> ST s () #-}
{-# SPECIALISE FixedSort.sort3       :: PM.MVector s Word64 -> ST s () #-}
{-# SPECIALISE FixedSort.sort4       :: PM.MVector s Word64 -> ST s () #-}

{-# SPECIALISE FixedSort.bitonicSort :: Int -> PM.MVector s SortKey -> ST s () #-}
{-# SPECIALISE VExt.qsort            :: PM.MVector s SortKey -> ST s () #-}
{-# SPECIALISE FixedSort.sort3       :: PM.MVector s SortKey -> ST s () #-}
{-# SPECIALISE FixedSort.sort4       :: PM.MVector s SortKey -> ST s () #-}

{-# SPECIALISE FixedSort.bitonicSort :: Int -> P.MVector s Char -> ST s () #-}
{-# SPECIALISE VExt.qsort            :: P.MVector s Char -> ST s () #-}
{-# SPECIALISE FixedSort.sort3       :: P.MVector s Char -> ST s () #-}
{-# SPECIALISE FixedSort.sort4       :: P.MVector s Char -> ST s () #-}

{-# NOINLINE qsortWord64 #-}
qsortWord64 :: PM.MVector s Word64 -> ST s ()
qsortWord64 = VExt.qsort

{-# NOINLINE qsortSortKey #-}
qsortSortKey :: PM.MVector s SortKey -> ST s ()
qsortSortKey = VExt.qsort

{-# NOINLINE sortVectorUnsafeChar #-}
sortVectorUnsafeChar :: P.Vector Char -> P.Vector Char
sortVectorUnsafeChar = VExt.sortVectorUnsafe
