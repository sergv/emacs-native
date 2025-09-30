-- |
-- Module:     Data.UnicodeUtils.Tests
-- Copyright:  (c) Sergey Vinokurov 2025
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE OverloadedStrings #-}

module Data.UnicodeUtils.Tests (tests) where

import Control.Monad
import Data.Text qualified as T
import Prettyprinter.Combinators
import Test.Tasty
import Test.Tasty.HUnit

import Data.UnicodeUtils

tests :: TestTree
tests = testGroup "Data.UnicodeUtils.Tests"
  [ testGroup "utf8LengthByLeader"
      [ testCase (show input ++ " -> " ++ show output) $
          checkEqual output (utf8LengthByLeader input)
      | (input, output) <-
          [ (0,   1)
          , (10,  1)
          , (32,  1)
          , (127, 1)
          , (128, 1)
          , (128 + 64, 2)
          , (128 + 64 + 32, 3)
          , (128 + 64 + 32 + 16, 4)
          ]
      ]
  ]

checkEqual
  :: (Eq a, Show a, Pretty a)
  => a      -- ^ The expected value
  -> a      -- ^ The actual value
  -> Assertion
checkEqual actual expected = unless (actual == expected) $ assertFailure msg
  where
    msg = T.unpack $ render $ ppDictHeader "Different results"
      [ "actual"   --> actual
      , "expected" --> expected
      ]

