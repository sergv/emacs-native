----------------------------------------------------------------------------
-- |
-- Module      :  EmacsNativeTest
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module EmacsNativeTest (main) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import Test.Tasty
import Test.Tasty.HUnit

import Data.FuzzyMatch

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [fuzzyMatchTests]

foobarHeatmap :: U.Vector Int
foobarHeatmap = U.fromList [84, -2, -3, -4, -5, -5]

noMatch :: Match
noMatch = Match
  { mScore = 0
  , mPositions = 0 :| []
  }

fuzzyMatchTests :: TestTree
fuzzyMatchTests = testGroup "fuzzy match"
  [ mkTestCase "foo" "foobar" foobarHeatmap $ Match
      { mScore = 214
      , mPositions = 0 :| [1, 2]
      }
  , mkTestCase "fo" "foobar" foobarHeatmap $ Match
      { mScore = 142
      , mPositions = 0 :| [1]
      }
  , mkTestCase "oob" "foobar" foobarHeatmap $ Match
      { mScore = 126
      , mPositions = 1 :| [2, 3]
      }
  , mkTestCase "ooba" "foobar" foobarHeatmap $ Match
      { mScore = 211
      , mPositions = 1 :| [2, 3, 4]
      }
  , mkTestCase "or" "foobar" foobarHeatmap $ Match
      { mScore = (-7)
      , mPositions = 1 :| [5]
      }
  , mkTestCase "oor" "foobar" foobarHeatmap $ Match
      { mScore = 50
      , mPositions = 1 :| [2, 5]
      }
  , mkTestCase "x" "foobar" foobarHeatmap noMatch
  , mkTestCase "fooxar" "foobar" foobarHeatmap noMatch

  , mkTestCase "aaaaaaaaaa" (T.replicate 100 "a") (U.replicate 100 1) $ Match
      { mScore = 865
      , mPositions = NE.fromList [90..99]
      }
  ]
  where
    mkTestCase :: Text -> Text -> U.Vector Int -> Match -> TestTree
    mkTestCase needle haystack haystackHeatmap result =
      testCase ("match '" <> T.unpack needle <> "' against '" <> T.unpack haystack <> "'") $
        fuzzyMatch haystackHeatmap needle haystack @?= result

