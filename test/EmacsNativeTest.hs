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

import Control.LensBlaze.Tests qualified
import Data.FuzzyMatch.Tests qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ Control.LensBlaze.Tests.tests
  , Data.FuzzyMatch.Tests.tests
  ]

