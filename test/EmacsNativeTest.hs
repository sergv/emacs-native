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

import System.IO.CodePage
import Test.Tasty

import Data.UnicodeUtils.Tests qualified
import Control.LensBlaze.Tests qualified
import Data.FuzzyMatch.Tests qualified
import Data.Filesystem.Grep.Tests qualified

main :: IO ()
main = withCP65001 $ defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ Control.LensBlaze.Tests.tests
  , Data.FuzzyMatch.Tests.tests
  , Data.Filesystem.Grep.Tests.tests
  , Data.UnicodeUtils.Tests.tests
  ]

