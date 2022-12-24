----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Ext.Tests
-- Copyright   :  (c) sergey 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Vector.Ext.Tests (tests) where

import Data.List qualified as L
import Data.Vector qualified as V
import Data.Vector.Ext qualified as VE
import Data.Vector.Unboxed qualified as U
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests = testGroup "Data.Vector.Ext.Tests"
  [ sortTests
  ]

sortTests :: TestTree
sortTests = testGroup "sort tests"
  [ localOption (QC.QuickCheckTests 10000) $
    QC.testProperty "Data.Vector Int sorting" $
      \(xs :: [Int]) ->
        V.toList (VE.sortVectorUnsafe (V.fromList xs)) == L.sort xs
  , localOption (QC.QuickCheckTests 10000) $
    QC.testProperty "Data.Vector (Int, Int) sorting" $
      \(xs :: [(Int, Int)]) ->
        V.toList (VE.sortVectorUnsafe (V.fromList xs)) == L.sort xs
  , localOption (QC.QuickCheckTests 10000) $
    QC.testProperty "Data.Vector.Unboxed Int sorting" $
      \(xs :: [Int]) ->
        U.toList (VE.sortVectorUnsafe (U.fromList xs)) == L.sort xs
  , localOption (QC.QuickCheckTests 10000) $
    QC.testProperty "Data.Vector.Unboxed (Int, Int) sorting" $
      \(xs :: [(Int, Int)]) ->
        U.toList (VE.sortVectorUnsafe (U.fromList xs)) == L.sort xs
  ]
