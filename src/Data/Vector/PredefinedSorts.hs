----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.PredefinedSorts
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Vector.PredefinedSorts
  ( sortInt32
  , sortWord64
  , sortVectorUnsafeChar
  , sortSortKeyPar
  ) where

import Control.Monad.ST
import Data.FuzzyMatch.SortKey
import Data.Int
import Data.Vector.Primitive qualified as P
import Data.Vector.Primitive.Mutable qualified as PM
import Data.Word

import Data.Vector.Algorithms.Quicksort.Parameterised qualified as Quick

{-# NOINLINE sortInt32 #-}
sortInt32 :: PM.MVector s Int32 -> ST s ()
sortInt32 = Quick.sortInplaceFM Quick.Sequential (Quick.Median3 @Int32)

{-# NOINLINE sortWord64 #-}
sortWord64 :: PM.MVector s Word64 -> ST s ()
sortWord64 = Quick.sortInplaceFM Quick.Sequential (Quick.Median3 @Word64)

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
