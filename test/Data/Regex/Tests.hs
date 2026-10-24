-- |
-- Module:     Data.Regex.Tests
-- Copyright:  (c) Sergey Vinokurov 2026
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE OverloadedStrings #-}

module Data.Regex.Tests (tests) where

import Data.Text (Text)
import Data.Text qualified as T
import System.OsPath
import Test.Tasty
import Test.Tasty.HUnit

import Data.Regex

tests :: TestTree
tests = testGroup "Data.Regex.Tests"
  [ globTests
  ]

globTests :: TestTree
globTests = testGroup "glob tests"
  [ testCase (T.unpack ("globs " <> T.pack (show globs) <> " " <> (if outcome then "match" else "don’t match") <> " input ‘" <> T.pack input <> "’")) $ do
      globs' <- fileGlobsToRegex (globs :: [Text])
      input' <- encodeUtf input
      reSetMatchesOsPath globs' input' @?= outcome
  | (globs, input, outcome) <-
      [ (["*"],          "abc",                 True)
      , (["*"],          "",                    True)
      , ([],             "",                    False)
      , ([""],           "",                    True)
      , (["abc"],        "abc",                 True)
      , (["a*c"],        "abc",                 True)
      , (["a*d"],        "abc",                 False)
      , (["a*c"],        "abcd",                False)
      , (["a*c", "a*d"], "abcd",                True)
      , (["*/foo*/*"],   "bar/foobar/baz",      True)
      , (["*/foo*/*"],   "bar/foobar/baz/quux", False)
      , (["*/foo*/**"],  "bar/foobar/baz/quux", True)
      ]
  ]
