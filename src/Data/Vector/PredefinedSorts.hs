----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.PredefinedSorts
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-orphans #-}

{-# OPTIONS_GHC -O2 #-}

-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-uniques -dsuppress-coercions -dppr-cols200 -ddump-to-file #-}

{-# OPTIONS_GHC -ddump-simpl -dsuppress-uniques -dsuppress-idinfo -dsuppress-module-prefixes -dsuppress-type-applications -dsuppress-coercions -dppr-cols200 -dsuppress-type-signatures -ddump-to-file #-}

module Data.Vector.PredefinedSorts
  ( qsortInt32
  , qsortWord64
  , sortVectorUnsafeChar
  ) where

-- import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Fork
import Control.Monad.ST
-- import Data.FuzzyMatch.SortKey
import Data.Int
import Data.Median
import Data.Vector.Ext qualified as VExt
import Data.Vector.FixedSort qualified as FixedSort
import Data.Vector.Primitive qualified as P
import Data.Vector.Primitive.Mutable qualified as PM
import Data.Word

import Data.Vector.PredefinedSortsCommon ()

{-# SPECIALISE VExt.qsort            :: Sequential -> Median3 Int32  -> PM.MVector s Int32  -> ST s () #-}

{-# SPECIALISE VExt.qsort            :: Sequential -> Median3 Word64 -> PM.MVector s Word64 -> ST s () #-}

{-# SPECIALISE FixedSort.bitonicSort :: Int -> P.MVector s Char -> ST s () #-}
{-# SPECIALISE VExt.qsort            :: Sequential -> Median3 Char -> P.MVector s Char -> ST s () #-}
{-# SPECIALISE FixedSort.sort3       :: P.MVector s Char -> ST s () #-}
{-# SPECIALISE FixedSort.sort4       :: P.MVector s Char -> ST s () #-}

{-# NOINLINE qsortInt32 #-}
qsortInt32 :: PM.MVector s Int32 -> ST s ()
qsortInt32 = VExt.qsort Sequential (Median3 @Int32)

{-# NOINLINE qsortWord64 #-}
qsortWord64 :: PM.MVector s Word64 -> ST s ()
qsortWord64 = VExt.qsort Sequential (Median3 @Word64)

{-# NOINLINE sortVectorUnsafeChar #-}
sortVectorUnsafeChar :: P.Vector Char -> P.Vector Char
sortVectorUnsafeChar = VExt.sortVectorUnsafe
