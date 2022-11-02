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

import Data.Char
import Data.Int
import Data.IntSet qualified as IS
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Primitive.PrimArray
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

import Data.FuzzyMatch

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [fuzzyMatchTests, heatMap, heatMapGrouping]

foobarHeatmap :: PrimArray Int32
foobarHeatmap = primArrayFromList [84, -2, -3, -4, -5, -5]

noMatch :: Match
noMatch = Match
  { mScore     = (-1)
  , mPositions = StrIdx (-1) :| []
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

  , mkTestCase "aaaaaaaaaa" (T.replicate 100 "a") (replicatePrimArray 100 1) Match
      { mScore     = 865
      , mPositions = NE.fromList [StrIdx 90..StrIdx 99]
      }
  , mkTestCase "aaaaaaaaaa" (T.replicate 200 "a") (replicatePrimArray 200 1) Match
      { mScore     = 865
      , mPositions = NE.fromList [StrIdx 190..StrIdx 199]
      }
  , let haystack = "sys/dev/acpica/Osd/OsdTable.c" :: Text in
      mkTestCase "cat.c" haystack (computeHeatMap haystack mempty) Match
        { mScore     = 142
        , mPositions = NE.fromList (map StrIdx [12, 13, 22, 27, 28])
        }
  , let haystack = "/home/user/projects/Data/Vector.hs" :: Text in
    mkTestCase "vector" haystack (computeHeatMap haystack mempty) Match
      { mScore     = 397
      , mPositions = fmap StrIdx $ 25 :| [26, 27, 28, 29, 30]
      }
  , let haystack = "all-packages/vector-th-unbox-0.2.2/Data/Vector/Unboxed/Deriving.hs" :: Text in
    mkTestCase "vector.hs" haystack (computeHeatMap haystack mempty) Match
      { mScore     = 414
      , mPositions = fmap StrIdx $ 13 :| [14, 15, 16, 17, 18, 63, 64, 65]
      }
  ]
  where
    mkTestCase :: Text -> Text -> PrimArray Int32 -> Match -> TestTree
    mkTestCase needle haystack haystackHeatmap result =
      testCase (T.unpack $ "match '" <> needle <> "' against '" <> haystack <> "'") $
        fuzzyMatch haystackHeatmap needle (prepareNeedle needle) haystack @?= result

fi32 :: Integral a => a -> Int32
fi32 = fromIntegral

heatMap :: TestTree
heatMap = testGroup "Heatmap"
  [ mkTestCase "foo" mempty [84, - 2, - 2]
  , mkTestCase "bar" mempty [84, - 2, - 2]
  , mkTestCase "foo.bar" mempty [83, -3, -4, -5, 35, -6, -6]
  , mkTestCase "foo/bar/baz" mempty [82, -4, -5, -6, 79, -7, -8, -9, 76, -10, -10]
  , mkTestCase "foo/bar/baz" (primArrayFromList [fi32 $ ord '/']) [41, -45, -46, -47, 39, -47, -48, -49, 79, -7, -7]
  , mkTestCase
      "foo/bar+quux/fizz.buzz/frobnicate/frobulate"
      mempty
      [78, -8, -9, -10, 75, -11, -12, -13, 72, -14, -15, -16, -17, 69, -17, -18, -19, -20, 21, -20, -21, -22, -23, 63, -23, -24, -25, -26, -27, -28, -29, -30, -31, -32, 60, -26, -27, -28, -29, -30, -31, -32, -32]
  , mkTestCase
      "foo/bar+quux/fizz.buzz"
      (primArrayFromList [fi32 $ ord '/'])
      [41, -45, -46, -47, 39, -47, -48, -49, 36, -50, -51, -52, -53, 78, -8, -9, -10, -11, 30, -11, -12, -12]
  , mkTestCase
      "foo/bar+quux/fizz.buzz/frobnicate/frobulate"
      (primArrayFromList [fi32 $ ord '/'])
      [37, -49, -50, -51, 35, -51, -52, -53, 32, -54, -55, -56, -57, 36, -50, -51, -52, -53, -12, -53, -54, -55, -56, 37, -49, -50, -51, -52, -53, -54, -55, -56, -57, -58, 77, -9, -10, -11, -12, -13, -14, -15, -15]
  , mkTestCase
      "foo/bar+quux/fizz.buzz//frobnicate/frobulate"
      (primArrayFromList [fi32 $ ord '/'])
      [35, -51, -52, -53, 33, -53, -54, -55, 30, -56, -57, -58, -59, 34, -52, -53, -54, -55, -14, -55, -56, -57, -58, -50, 36, -50, -51, -52, -53, -54, -55, -56, -57, -58, -59, 76, -10, -11, -12, -13, -14, -15, -16, -16]
  , mkTestCase
      "foo/bar+quux/fizz.buzz//frobnicate/frobulate"
      (primArrayFromList (L.sort [fi32 $ ord '/', fi32 $ ord 'u']))
      [27, -59, -60, -61, 25, -61, -62, -63, 22, -64, -59, -58, -58, 28, -58, -59, -60, -61, -20, -61, -56, -56, -56, -55, 31, -55, -56, -57, -58, -59, -60, -61, -62, -63, -64, 72, -14, -15, -16, -17, -52, -52, -52, -51]
  , mkTestCase
      "foo/barQuux/fizzBuzz//frobnicate/frobulate"
      mempty
      [80, -6, -7, -8, 77, -9, -10, 74, -12, -13, -14, -15, 71, -15, -16, -17, 68, -18, -19, -20, -21, -22, 65, -21, -22, -23, -24, -25, -26, -27, -28, -29, -30, 62, -24, -25, -26, -27, -28, -29, -30, -30]
  , mkTestCase
      "foo//bar"
      mempty
      [83, -3, -4, -5, -6, 80, -6, -6]
  , mkTestCase
      "foo//bar"
      (primArrayFromList [fi32 $ ord '/'])
      [41, -45, -46, -47, -46, 79, -7, -7]
  ]
  where
    mkTestCase :: Text -> PrimArray Int32 -> [Int32] -> TestTree
    mkTestCase str groupSeps result =
      testCase (T.unpack $ "Heatmap of '" <> str <> "'" <> seps) $
        computeHeatMap str groupSeps @?= primArrayFromList result
      where
        seps
          | sizeofPrimArray groupSeps == 0 = mempty
          | otherwise                      =
            " with seps " <> T.intercalate ", " (map (T.singleton . chr . fromIntegral) $ primArrayToList groupSeps)

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
  , mkTestCase "foo/bar" (primArrayFromList [fi32 $ ord '/'])
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
  , mkTestCase "foo/bar/baz" (primArrayFromList [fi32 $ ord '/'])
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
      (primArrayFromList [fi32 $ ord '/'])
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
    mkTestCase :: Text -> PrimArray Int32 -> [HeatMapGroup] -> TestTree
    mkTestCase str groupSeps expectedRes =
      testCase (T.unpack ("groups of '" <> str <> "'" <> seps)) $
        computeGroupsAndInitScores str groupSeps @?= (fromIntegral $ length expectedRes, expectedRes)
      where
        seps
          | sizeofPrimArray groupSeps == 0 = mempty
          | otherwise                      =
            " with seps " <> T.intercalate ", " (map (T.singleton . chr . fromIntegral) $ primArrayToList groupSeps)
