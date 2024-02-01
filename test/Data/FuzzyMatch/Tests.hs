----------------------------------------------------------------------------
-- |
-- Module      :  Data.FuzzyMatch.Tests
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}

module Data.FuzzyMatch.Tests (tests) where

import Control.Monad.ST
import Data.Char
import Data.Int
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Primitive.PrimArray
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable
import Data.Vector.Primitive qualified as P
import Test.Tasty
import Test.Tasty.HUnit

import Data.FuzzyMatch

tests :: TestTree
tests = testGroup "Data.FuzzyMatch.Tests"
  [ fuzzyMatchTests
  , fuzzyMatchMultipleTests
  , heatMap
  -- , heatMapGrouping
  ]

foobarHeatmap :: P.Vector Heat
foobarHeatmap = P.fromList $ map Heat [84, -2, -3, -4, -5, -5]

fuzzyMatchTests :: TestTree
fuzzyMatchTests = testGroup "fuzzy match" $
  [ mkTestCase "foo" "foobar" (\_ -> Heatmap <$> P.unsafeThaw foobarHeatmap) $ Just Match
      { mScore     = 214
      , mPositions = StrCharIdx 0 :| [StrCharIdx 1, StrCharIdx 2]
      }
  , mkTestCase "fo" "foobar" (\_ -> Heatmap <$> P.unsafeThaw foobarHeatmap) $ Just Match
      { mScore     = 142
      , mPositions = StrCharIdx 0 :| [StrCharIdx 1]
      }
  , mkTestCase "oob" "foobar" (\_ -> Heatmap <$> P.unsafeThaw foobarHeatmap) $ Just Match
      { mScore     = 126
      , mPositions = StrCharIdx 1 :| [StrCharIdx 2, StrCharIdx 3]
      }
  , mkTestCase "ooba" "foobar" (\_ -> Heatmap <$> P.unsafeThaw foobarHeatmap) $ Just Match
      { mScore     = 211
      , mPositions = StrCharIdx 1 :| [StrCharIdx 2, StrCharIdx 3, StrCharIdx 4]
      }
  , mkTestCase "or" "foobar" (\_ -> Heatmap <$> P.unsafeThaw foobarHeatmap) $ Just Match
      { mScore     = (-7)
      , mPositions = StrCharIdx 1 :| [StrCharIdx 5]
      }
  , mkTestCase "oor" "foobar" (\_ -> Heatmap <$> P.unsafeThaw foobarHeatmap) $ Just Match
      { mScore     = 50
      , mPositions = StrCharIdx 1 :| [StrCharIdx 2, StrCharIdx 5]
      }
  , mkTestCase "x" "foobar" (\_ -> Heatmap <$> P.unsafeThaw foobarHeatmap) Nothing
  , mkTestCase "fooxar" "foobar" (\_ -> Heatmap <$> P.unsafeThaw foobarHeatmap) Nothing

  , mkTestCase "aaaaaaaaaa" (T.replicate 100 "a") (constHeatMap 100) $ Just Match
      { mScore     = 865
      , mPositions = NE.fromList [StrCharIdx 90..StrCharIdx 99]
      }
  , mkTestCase "aaaaaaaaaa" (T.replicate 200 "a") (constHeatMap 200) $ Just Match
      { mScore     = 865
      , mPositions = NE.fromList [StrCharIdx 190..StrCharIdx 199]
      }
  , let haystack = "sys/dev/acpica/Osd/OsdTable.c" :: Text in
      mkTestCase "cat.c" haystack (mkHeatMap haystack) $ Just Match
        { mScore     = 142
        , mPositions = NE.fromList (map StrCharIdx [12, 13, 22, 27, 28])
        }
  , let haystack = "/home/user/projects/Data/Vector.hs" :: Text in
    mkTestCase "vector" haystack (mkHeatMap haystack) $ Just Match
      { mScore     = 397
      , mPositions = fmap StrCharIdx $ 25 :| [26, 27, 28, 29, 30]
      }
  , let haystack = "all-packages/vector-th-unbox-0.2.2/Data/Vector/Unboxed/Deriving.hs" :: Text in
    mkTestCase "vector.hs" haystack (mkHeatMap haystack) $ Just Match
      { mScore     = 414
      , mPositions = fmap StrCharIdx $ 13 :| [14, 15, 16, 17, 18, 63, 64, 65]
      }
  , let haystack = "все-пакеты/vector-th-unbox-0.2.2/Data/Вектор/Unboxed/Deriving.hs" :: Text in
    mkTestCase "Вектор.hs" haystack (mkHeatMap haystack) $ Just Match
      { mScore     = 288
      , mPositions = fmap StrCharIdx $ 38 :| [39, 40, 41, 42, 43, 61, 62, 63]
      }

  , let haystack = "src/Data/FuzzyMatch.hs" :: Text in
    mkTestCase "data fuz" haystack (mkHeatMap haystack) $ Just Match
      { mScore     = 463
      , mPositions = fmap StrCharIdx $ 4 :| [5, 6, 7, 9, 10, 11]
      }
  , let haystack = "src/Data/FuzzyMatch.hs" :: Text in
    mkTestCase "data fuzz" haystack (mkHeatMap haystack) $ Just Match
      { mScore     = 540
      , mPositions = fmap StrCharIdx $ 4 :| [5, 6, 7, 9, 10, 11, 12]
      }

  , let haystack = "_build/generated/Aleph/Base.hs" :: Text in
    mkTestCase "aleph base" haystack (mkHeatMap haystack) $ Just Match
      { mScore     = 598
      , mPositions = fmap StrCharIdx $ 17 :| [18, 19, 20, 21, 23, 24, 25, 26]
      }
  , let haystack = "_build/generated/Aleph/Base.hs" :: Text in
    mkTestCase "aleph baseh" haystack (mkHeatMap haystack) Nothing

  , let haystack = "all-packages/vector-th-unbox-0.2.2/Data/Vector/Unboxed/Deriving.hs" :: Text in
    mkTestCase "deriv vec" haystack (mkHeatMap haystack) $ Just Match
      { mScore     = 335
      , mPositions = fmap StrCharIdx $ 13 :| [14, 15, 55, 56, 57, 58, 59]
      }
  , let haystack = "abc/baz/abc/foo/abc/bar/abc" :: Text in
    mkTestCase "foo bar baz" haystack (mkHeatMap haystack) $ Just Match
      { mScore     = 507
      , mPositions = fmap StrCharIdx $ 4 :| [5, 6, 12, 13, 14, 20, 21, 22]
      }
  , let haystack = "abfc/baz/abrc/foo/aboc/bar/abcb" :: Text in
    mkTestCase "foo bar baz frob" haystack (mkHeatMap haystack) Nothing
  , let haystack = "+foo+bar+" :: Text in
    mkTestCase "foo bar baz" haystack (mkHeatMap haystack) Nothing
  ] ++
  [ mkTestCase "fooo xyz" haystack (mkHeatMap haystack) Nothing
  | haystack <-
    [ "x+fooo+yz"
    , "xy+fooo+z"
    , "zyx+fooo"
    ]
  ] ++
  [ mkTestCase "foo bar xyz" haystack (mkHeatMap haystack) Nothing
  | haystack <- addPrefixesSuffixes
      [ "x+foo+y+bar+z"
      , "xy+foo+bar+z"
      , "x+foo+bar+yz"
      , "foo+xy+bar+z"
      , "foo+x+bar+yz"
      , "xy+foo+z+bar"
      , "x+foo+yz+bar"
      ]
  ]
  where
    addPrefixesSuffixes xs = xs ++ map ("W" <>) xs ++ map (<> "W") xs

    constHeatMap :: Int -> forall s. ReusableState s -> ST s (Heatmap s)
    constHeatMap len _ = Heatmap <$> P.unsafeThaw (P.replicate len (Heat 1))

    mkHeatMap :: Text -> forall s. ReusableState s -> ST s (Heatmap s)
    mkHeatMap haystack store = computeHeatmap store haystack (T.length haystack) mempty

    mkTestCase :: Text -> Text -> (forall s. ReusableState s -> ST s (Heatmap s)) -> Maybe Match -> TestTree
    mkTestCase needle haystack mkHeatmap result =
      testCase (T.unpack $ "match ‘" <> needle <> "’ against ‘" <> haystack <> "’") $ do
        let match = runST $ do
              store   <- mkReusableState (T.length needle)
              heatmap <- mkHeatmap store
              fuzzyMatch store heatmap needle haystack
        match @?= result

fuzzyMatchMultipleTests :: TestTree
fuzzyMatchMultipleTests = testGroup "fuzzy match multiple"
  [ mkTestCase "foo" ["foobar", "foobaz", "quux", "fqouuxo"] [Just 214, Just 214, Nothing, Just 75]
  , mkTestCase "vector.hs" ["local-store/ghc-9.4.2/vector-space-0.16-6c2632778a7166806a878ce1c082a8cd55db17dc183ef6153dc43f8064939746/share/doc/html/meta.json", "/home/sergey/projects/haskell/packages/local-store/ghc-9.4.2/mime-types-0.1.1.0-36574ed6c6ba4b463c91ac91e7334e6d64c7e64484e986bb0ef24ae7064fefb6/cabal-hash.txt", "local-store/ghc-9.4.2/mime-types-0.1.1.0-36574ed6c6ba4b463c91ac91e7334e6d64c7e64484e986bb0ef24ae7064fefb6/cabal-hash.txt"] [Just 228, Nothing, Nothing]
  , mkTestCase "Trie.hs" ["TradeScriptAux/Fides.hs", "Packages/utils/src/Interlude.hs", "src/Tools/XtrmPricer/XtrmPricer.hs", "Packages/BackendMatrix/src/BackendMatrix/Core.hs",  "src/BackendTaxonomy/Tests/Facts/TargetAccrualEarlyRedemptionTestFact.hs", "Packages/utils/src/Utils/Trie.hs"] [Just 238, Nothing, Just 191, Nothing, Just 114, Just 523]
  ]
  where
    mkTestCase :: Text -> [Text] -> [Maybe Int] -> TestTree
    mkTestCase needle haystacks expectedScores =
      testCase (T.unpack $ "match ‘" <> needle <> "’ against ‘" <> T.pack (show haystacks) <> "’") $ do
        let matches = runST $ do
              store <- mkReusableState (T.length needle)
              for haystacks $ \haystack -> do
                heatmap <- computeHeatmap store haystack (T.length haystack) mempty
                !match  <- fuzzyMatch store heatmap needle haystack
                pure $ mScore <$> match
        matches @?= expectedScores


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
  , mkTestCase
      ".emacs.d"
      (primArrayFromList [fi32 $ ord '.'])
      [41, -6, -47, -48, -49, -50, -51, 35]
  ]
  where
    mkTestCase :: Text -> PrimArray Int32 -> [Int32] -> TestTree
    mkTestCase str groupSeps result =
      testCase (T.unpack $ "Heatmap of ‘" <> str <> "’" <> seps) $ do
        let heatmap = runST $ do
              store      <- mkReusableState 3
              Heatmap hm <- computeHeatmap store str (T.length str) groupSeps
              P.unsafeFreeze hm
        heatmap @?= P.fromList (map Heat result)
      where
        seps
          | sizeofPrimArray groupSeps == 0 = mempty
          | otherwise                      =
            " with seps " <> T.intercalate ", " (map (T.singleton . chr . fromIntegral) $ primArrayToList groupSeps)

-- heatMapGrouping :: TestTree
-- heatMapGrouping = testGroup "Grouping for heatmap computation"
--   [ mkTestCase "foo" mempty $ (:[]) HeatmapGroup
--     { hmgStart           = StrCharIdx (-1)
--     , hmgEnd             = StrCharIdx 2
--     , hmgWordCount       = 1
--     , hmgWordIndices     = [StrCharIdx 0]
--     , hmgWordIndicesSize = 1
--     , hmgIsBasePath      = True
--     }
--   , mkTestCase "bar" mempty $ (:[]) HeatmapGroup
--     { hmgStart           = StrCharIdx (-1)
--     , hmgEnd             = StrCharIdx 2
--     , hmgWordCount       = 1
--     , hmgWordIndices     = [StrCharIdx 0]
--     , hmgWordIndicesSize = 1
--     , hmgIsBasePath      = True
--     }
--   , mkTestCase "foo.bar" mempty $ (:[]) HeatmapGroup
--     { hmgStart           = StrCharIdx (-1)
--     , hmgEnd             = StrCharIdx 6
--     , hmgWordCount       = 2
--     , hmgWordIndices     = [StrCharIdx 4, StrCharIdx 0]
--     , hmgWordIndicesSize = 2
--     , hmgIsBasePath      = True
--     }
--   , mkTestCase "foo+bar" mempty $ (:[]) HeatmapGroup
--     { hmgStart           = StrCharIdx (-1)
--     , hmgEnd             = StrCharIdx 6
--     , hmgWordCount       = 2
--     , hmgWordIndices     = [StrCharIdx 4, StrCharIdx 0]
--     , hmgWordIndicesSize = 2
--     , hmgIsBasePath      = True
--     }
--   , mkTestCase "foo/bar/baz" mempty $ (:[]) HeatmapGroup
--     { hmgStart           = StrCharIdx (-1)
--     , hmgEnd             = StrCharIdx 10
--     , hmgWordCount       = 3
--     , hmgWordIndices     = [StrCharIdx 8, StrCharIdx 4, StrCharIdx 0]
--     , hmgWordIndicesSize = 3
--     , hmgIsBasePath      = True
--     }
--   , mkTestCase "foo/bar" (primArrayFromList [fi32 $ ord '/'])
--     [ HeatmapGroup
--       { hmgStart           = StrCharIdx (-1)
--       , hmgEnd             = StrCharIdx 2
--       , hmgWordCount       = 1
--       , hmgWordIndices     = [StrCharIdx 0]
--       , hmgWordIndicesSize = 1
--       , hmgIsBasePath      = False
--       }
--     , HeatmapGroup
--       { hmgStart           = StrCharIdx 3
--       , hmgEnd             = StrCharIdx 6
--       , hmgWordCount       = 1
--       , hmgWordIndices     = [StrCharIdx 4]
--       , hmgWordIndicesSize = 1
--       , hmgIsBasePath      = True
--       }
--     ]
--   , mkTestCase "foo/bar/baz" (primArrayFromList [fi32 $ ord '/'])
--     [ HeatmapGroup
--       { hmgStart           = StrCharIdx (-1)
--       , hmgEnd             = StrCharIdx 2
--       , hmgWordCount       = 1
--       , hmgWordIndices     = [StrCharIdx 0]
--       , hmgWordIndicesSize = 1
--       , hmgIsBasePath      = False
--       }
--     , HeatmapGroup
--       { hmgStart           = StrCharIdx 3
--       , hmgEnd             = StrCharIdx 6
--       , hmgWordCount       = 1
--       , hmgWordIndices     = [StrCharIdx 4]
--       , hmgWordIndicesSize = 1
--       , hmgIsBasePath      = False
--       }
--     , HeatmapGroup
--       { hmgStart           = StrCharIdx 7
--       , hmgEnd             = StrCharIdx 10
--       , hmgWordCount       = 1
--       , hmgWordIndices     = [StrCharIdx 8]
--       , hmgWordIndicesSize = 1
--       , hmgIsBasePath      = True
--       }
--     ]
--   , mkTestCase "foo/bar+quuz" mempty
--       [ HeatmapGroup
--           { hmgStart           = StrCharIdx (-1)
--           , hmgEnd             = StrCharIdx 11
--           , hmgWordCount       = 3
--           , hmgWordIndices     = [StrCharIdx 8, StrCharIdx 4, StrCharIdx 0]
--           , hmgWordIndicesSize = 3
--           , hmgIsBasePath      = True
--           }
--       ]
--   , mkTestCase
--       "foo/bar+quux/fizz.buzz/frobnicate/frobulate"
--       (primArrayFromList [fi32 $ ord '/'])
--       [ HeatmapGroup
--           { hmgStart           = StrCharIdx (-1)
--           , hmgEnd             = StrCharIdx 2
--           , hmgWordCount       = 1
--           , hmgWordIndices     = [StrCharIdx 0]
--           , hmgWordIndicesSize = 1
--           , hmgIsBasePath      = False
--           }
--       , HeatmapGroup
--           { hmgStart           = StrCharIdx 3
--           , hmgEnd             = StrCharIdx 11
--           , hmgWordCount       = 2
--           , hmgWordIndices     = [StrCharIdx 8, StrCharIdx 4]
--           , hmgWordIndicesSize = 2
--           , hmgIsBasePath      = False
--           }
--       , HeatmapGroup
--           { hmgStart           = StrCharIdx 12
--           , hmgEnd             = StrCharIdx 21
--           , hmgWordCount       = 2
--           , hmgWordIndices     = [StrCharIdx 18, StrCharIdx 13]
--           , hmgWordIndicesSize = 2
--           , hmgIsBasePath      = False
--           }
--       , HeatmapGroup
--           { hmgStart           = StrCharIdx 22
--           , hmgEnd             = StrCharIdx 32
--           , hmgWordCount       = 1
--           , hmgWordIndices     = [StrCharIdx 23]
--           , hmgWordIndicesSize = 1
--           , hmgIsBasePath      = False
--           }
--       , HeatmapGroup
--           { hmgStart           = StrCharIdx 33
--           , hmgEnd             = StrCharIdx 42
--           , hmgWordCount       = 1
--           , hmgWordIndices     = [StrCharIdx 34]
--           , hmgWordIndicesSize = 1
--           , hmgIsBasePath      = True
--           }
--       ]
--   ]
--   where
--     mkTestCase :: Text -> PrimArray Int32 -> [HeatmapGroup] -> TestTree
--     mkTestCase str groupSeps expectedRes =
--       testCase (T.unpack ("groups of ‘" <> str <> "’" <> seps)) $
--         computeGroupsAndInitScores str (T.length str) groupSeps @?= (fromIntegral $ length expectedRes, expectedRes)
--       where
--         seps
--           | sizeofPrimArray groupSeps == 0 = mempty
--           | otherwise                      =
--             " with seps " <> T.intercalate ", " (map (T.singleton . chr . fromIntegral) $ primArrayToList groupSeps)
