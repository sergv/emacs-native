----------------------------------------------------------------------------
-- |
-- Module      :  EmacsNativeTest
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}

module EmacsNativeTest (main) where

import Test.Tasty

import Data.FuzzyMatch.Tests qualified
import Data.FuzzyMatchBaseline.Tests qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ Data.FuzzyMatch.Tests.tests
  , Data.FuzzyMatchBaseline.Tests.tests
  ]

