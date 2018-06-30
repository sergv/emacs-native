----------------------------------------------------------------------------
-- |
-- Module      :  EmacsNativeTest
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module EmacsNativeTest (main) where

import Data.Char
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
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
tests = testGroup "Tests" [fuzzyMatchTests, heatMap, heatMapGrouping]

foobarHeatmap :: U.Vector Int
foobarHeatmap = U.fromList [84, -2, -3, -4, -5, -5]

noMatch :: Match
noMatch = Match
  { mScore     = 0
  , mPositions = StrIdx 0 :| []
  }

fuzzyMatchTests :: TestTree
fuzzyMatchTests = testGroup "fuzzy match"
  [ mkTestCase "foo" "foobar" foobarHeatmap Match
      { mScore     = 214
      , mPositions = StrIdx 0 :| [StrIdx 1, StrIdx 2]
      }
  , mkTestCase "fo" "foobar" foobarHeatmap Match
      { mScore     = 142
      , mPositions = StrIdx 0 :| [StrIdx 1]
      }
  , mkTestCase "oob" "foobar" foobarHeatmap Match
      { mScore     = 126
      , mPositions = StrIdx 1 :| [StrIdx 2, StrIdx 3]
      }
  , mkTestCase "ooba" "foobar" foobarHeatmap Match
      { mScore     = 211
      , mPositions = StrIdx 1 :| [StrIdx 2, StrIdx 3, StrIdx 4]
      }
  , mkTestCase "or" "foobar" foobarHeatmap Match
      { mScore     = (-7)
      , mPositions = StrIdx 1 :| [StrIdx 5]
      }
  , mkTestCase "oor" "foobar" foobarHeatmap Match
      { mScore     = 50
      , mPositions = StrIdx 1 :| [StrIdx 2, StrIdx 5]
      }
  , mkTestCase "x" "foobar" foobarHeatmap noMatch
  , mkTestCase "fooxar" "foobar" foobarHeatmap noMatch

  , mkTestCase "aaaaaaaaaa" (T.replicate 100 "a") (U.replicate 100 1) Match
      { mScore     = 865
      , mPositions = NE.fromList [StrIdx 90..StrIdx 99]
      }
  , mkTestCase "aaaaaaaaaa" (T.replicate 200 "a") (U.replicate 200 1) Match
      { mScore     = 865
      , mPositions = NE.fromList [StrIdx 190..StrIdx 199]
      }
  ]
  where
    mkTestCase :: Text -> Text -> U.Vector Int -> Match -> TestTree
    mkTestCase needle haystack haystackHeatmap result =
      testCase ("match '" <> T.unpack needle <> "' against '" <> T.unpack haystack <> "'") $
        fuzzyMatch haystackHeatmap needle haystack @?= result

heatMap :: TestTree
heatMap = testGroup "Heatmap"
  [ mkTestCase "foo" mempty [84, - 2, - 2]
  , mkTestCase "bar" mempty [84, - 2, - 2]
  , mkTestCase "foo.bar" mempty [83, -3, -4, -5, 35, -6, -6]
  , mkTestCase "foo/bar/baz" mempty [82, -4, -5, -6, 79, -7, -8, -9, 76, -10, -10]
  , mkTestCase "foo/bar/baz" (IS.singleton (ord '/')) [41, -45, -46, -47, 39, -47, -48, -49, 79, -7, -7]
  , mkTestCase
      "foo/bar+quux/fizz.buzz/frobnicate/frobulate"
      (IS.singleton (ord '/'))
      [37, -49, -50, -51, 35, -51, -52, -53, 32, -54, -55, -56, -57, 36, -50, -51, -52, -53, -12, -53, -54, -55, -56, 37, -49, -50, -51, -52, -53, -54, -55, -56, -57, -58, 77, -9, -10, -11, -12, -13, -14, -15, -15]
  ]
  where
    mkTestCase :: Text -> IntSet -> [Int] -> TestTree
    mkTestCase str groupSeps result =
      testCase (T.unpack $ "Heatmap of '" <> str <> "'" <> seps) $
        computeHeatMap str groupSeps @?= U.fromList result
      where
        seps
          | IS.null groupSeps = mempty
          | otherwise         =
            " with seps " <> T.intercalate ", " (map (T.singleton . chr) $ IS.toList groupSeps)

heatMapGrouping :: TestTree
heatMapGrouping = testGroup "Grouping for heatmap computation"
  [ mkTestCase "foo" mempty $ (:[]) HeatMapGroup
    { hmgStart       = StrIdx (-1)
    , hmgEnd         = StrIdx 2
    , hmgWordCount   = 1
    , hmgWordIndices = IS.singleton 0
    , hmgIsBasePath  = True
    }
  , mkTestCase "bar" mempty $ (:[]) HeatMapGroup
    { hmgStart       = StrIdx (-1)
    , hmgEnd         = StrIdx 2
    , hmgWordCount   = 1
    , hmgWordIndices = IS.singleton 0
    , hmgIsBasePath  = True
    }
  , mkTestCase "foo.bar" mempty $ (:[]) HeatMapGroup
    { hmgStart       = StrIdx (-1)
    , hmgEnd         = StrIdx 6
    , hmgWordCount   = 2
    , hmgWordIndices = IS.fromList [4, 0]
    , hmgIsBasePath  = True
    }
  , mkTestCase "foo+bar" mempty $ (:[]) HeatMapGroup
    { hmgStart       = StrIdx (-1)
    , hmgEnd         = StrIdx 6
    , hmgWordCount   = 2
    , hmgWordIndices = IS.fromList [4, 0]
    , hmgIsBasePath  = True
    }
  , mkTestCase "foo/bar/baz" mempty $ (:[]) HeatMapGroup
    { hmgStart       = StrIdx (-1)
    , hmgEnd         = StrIdx 10
    , hmgWordCount   = 3
    , hmgWordIndices = IS.fromList [8, 4, 0]
    , hmgIsBasePath  = True
    }
  , mkTestCase "foo/bar" (IS.singleton (ord '/'))
    [ HeatMapGroup
      { hmgStart       = StrIdx (-1)
      , hmgEnd         = StrIdx 2
      , hmgWordCount   = 1
      , hmgWordIndices = IS.singleton 0
      , hmgIsBasePath  = False
      }
    , HeatMapGroup
      { hmgStart       = StrIdx 3
      , hmgEnd         = StrIdx 6
      , hmgWordCount   = 1
      , hmgWordIndices = IS.singleton 4
      , hmgIsBasePath  = True
      }
    ]
  , mkTestCase "foo/bar/baz" (IS.singleton (ord '/'))
    [ HeatMapGroup
      { hmgStart       = StrIdx (-1)
      , hmgEnd         = StrIdx 2
      , hmgWordCount   = 1
      , hmgWordIndices = IS.singleton 0
      , hmgIsBasePath  = False
      }
    , HeatMapGroup
      { hmgStart       = StrIdx 3
      , hmgEnd         = StrIdx 6
      , hmgWordCount   = 1
      , hmgWordIndices = IS.singleton 4
      , hmgIsBasePath  = False
      }
    , HeatMapGroup
      { hmgStart       = StrIdx 7
      , hmgEnd         = StrIdx 10
      , hmgWordCount   = 1
      , hmgWordIndices = IS.singleton 8
      , hmgIsBasePath  = True
      }
    ]
  , mkTestCase "foo/bar+quuz" mempty
      [ HeatMapGroup
          { hmgStart       = StrIdx (-1)
          , hmgEnd         = StrIdx 11
          , hmgWordCount   = 3
          , hmgWordIndices = IS.fromList [0, 4, 8]
          , hmgIsBasePath  = True
          }
      ]
  , mkTestCase
      "foo/bar+quux/fizz.buzz/frobnicate/frobulate"
      (IS.singleton (ord '/'))
      [ HeatMapGroup
          { hmgStart       = StrIdx (-1)
          , hmgEnd         = StrIdx 2
          , hmgWordCount   = 1
          , hmgWordIndices = IS.singleton 0
          , hmgIsBasePath  = False
          }
      , HeatMapGroup
          { hmgStart       = StrIdx 3
          , hmgEnd         = StrIdx 11
          , hmgWordCount   = 2
          , hmgWordIndices = IS.fromList [4, 8]
          , hmgIsBasePath  = False
          }
      , HeatMapGroup
          { hmgStart       = StrIdx 12
          , hmgEnd         = StrIdx 21
          , hmgWordCount   = 2
          , hmgWordIndices = IS.fromList [13, 18]
          , hmgIsBasePath  = False
          }
      , HeatMapGroup
          { hmgStart       = StrIdx 22
          , hmgEnd         = StrIdx 32
          , hmgWordCount   = 1
          , hmgWordIndices = IS.singleton 23
          , hmgIsBasePath  = False
          }
      , HeatMapGroup
          { hmgStart       = StrIdx 33
          , hmgEnd         = StrIdx 42
          , hmgWordCount   = 1
          , hmgWordIndices = IS.singleton 34
          , hmgIsBasePath  = True
          }
      ]
  ]
  where
    mkTestCase :: Text -> IntSet -> [HeatMapGroup] -> TestTree
    mkTestCase str groupSeps expectedRes =
      testCase (T.unpack ("groups of '" <> str <> "'" <> seps)) $
        computeGroupsAndInitScores str groupSeps @?= (length expectedRes, expectedRes)
      where
        seps
          | IS.null groupSeps = mempty
          | otherwise         =
            " with seps " <> T.intercalate ", " (map (T.singleton . chr) $ IS.toList groupSeps)
