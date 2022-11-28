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
  , sortVectorUnsafeChar
  , SortKey(..)
  , sortVectorUnsafeSortKey
  ) where

import Control.Monad.ST
import Data.Int
import Data.Ord
import Data.Vector qualified as V
import Data.Vector.Ext qualified as VExt
import Data.Vector.FixedSort qualified as FixedSort
import Data.Vector.Primitive qualified as P
import Data.Vector.Primitive.Mutable qualified as PM
import Data.Word

{-# SPECIALISE FixedSort.bitonicSort :: Int -> PM.MVector s Word64 -> ST s () #-}
{-# SPECIALISE VExt.qsort            :: PM.MVector s Word64 -> ST s () #-}
{-# SPECIALISE FixedSort.sort3       :: PM.MVector s Word64 -> ST s () #-}
{-# SPECIALISE FixedSort.sort4       :: PM.MVector s Word64 -> ST s () #-}

{-# SPECIALISE FixedSort.bitonicSort :: Int -> P.MVector s Char -> ST s () #-}
{-# SPECIALISE VExt.qsort            :: P.MVector s Char -> ST s () #-}
{-# SPECIALISE FixedSort.sort3       :: P.MVector s Char -> ST s () #-}
{-# SPECIALISE FixedSort.sort4       :: P.MVector s Char -> ST s () #-}

{-# SPECIALISE FixedSort.bitonicSort :: Int -> V.MVector s (SortKey a) -> ST s () #-}
{-# SPECIALISE VExt.qsort            :: V.MVector s (SortKey a) -> ST s () #-}
{-# SPECIALISE FixedSort.sort3       :: V.MVector s (SortKey a) -> ST s () #-}
{-# SPECIALISE FixedSort.sort4       :: V.MVector s (SortKey a) -> ST s () #-}

{-# NOINLINE qsortWord64 #-}
qsortWord64 :: PM.MVector s Word64 -> ST s ()
qsortWord64 = VExt.qsort

{-# NOINLINE sortVectorUnsafeChar #-}
sortVectorUnsafeChar :: P.Vector Char -> P.Vector Char
sortVectorUnsafeChar = VExt.sortVectorUnsafe

newtype SortKey v = SortKey { _unSortKey :: (Int32, Int, v) }

instance Eq (SortKey v) where
  SortKey (a, b, _) == SortKey (a', b', _) = a == a' && b == b'

instance Ord (SortKey v) where
  SortKey (a, b, _) `compare` SortKey (a', b', _) = Down a `compare` Down a' <> b `compare` b'

{-# NOINLINE sortVectorUnsafeSortKey #-}
sortVectorUnsafeSortKey :: V.Vector (SortKey a) -> V.Vector (SortKey a)
sortVectorUnsafeSortKey = VExt.sortVectorUnsafe
