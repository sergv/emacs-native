----------------------------------------------------------------------------
-- |
-- Module      :  FuzzyProf
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module FuzzyProf (main) where

import Control.DeepSeq
import Control.Exception
import Control.LensBlaze
import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.Int
import Data.List qualified as L
import Data.Ord
import Data.Primitive.PrimArray
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector qualified as V
import Data.Vector.PredefinedSorts
import Data.Vector.Primitive qualified as P
import Data.Vector.Primitive.Mutable qualified as PM
import System.Environment

import Data.FuzzyMatch qualified as FuzzyMatch
import Data.FuzzyMatch.SortKey
import Data.FuzzyMatchBaseline qualified as Sline

-- import Data.List qualified as L
-- import Data.FuzzyMatchBaseline qualified as Sline

{-# INLINE fi32 #-}
fi32 :: Integral a => a -> Int32
fi32 = fromIntegral

{-# NOINLINE doMatch #-}
doMatch :: PrimArray Int32 -> Text -> [Text] -> [(Int, Text)]
doMatch seps needle haystacks =
  map (\key -> (fromIntegral (getScore key), haystacks' `V.unsafeIndex` fromIntegral (view idxL key))) $ P.toList ys
  where
    needleChars = FuzzyMatch.prepareNeedle needle

    haystacks' :: V.Vector Text
    haystacks' = V.fromList haystacks

    ys :: P.Vector SortKey
    ys = runST $ do

      let !totalHaystacks = V.length haystacks'

      store <- FuzzyMatch.mkReusableState (T.length needle) needleChars

      scores <- PM.new totalHaystacks

      let go !n
            | n == totalHaystacks
            = pure ()
            | otherwise
            = do
              let !haystack    = haystacks' `V.unsafeIndex` n
                  !haystackLen = T.length haystack
              !match <- FuzzyMatch.fuzzyMatch' store (FuzzyMatch.computeHeatmap store haystack haystackLen seps) needle needleChars haystack
              PM.unsafeWrite scores n $!
                mkSortKey (FuzzyMatch.mScore match) (fromIntegral haystackLen) (fromIntegral n)
              go (n + 1)

      go 0
      qsortSortKey scores
      P.unsafeFreeze scores

      -- for (V.fromList xs) $ \str -> do
      --   match <-
      --     FuzzyMatch.fuzzyMatch'
      --       store
      --       (FuzzyMatch.computeHeatmap store str seps)
      --       needle
      --       needleChars
      --       str
      --   pure (fi32 $ FuzzyMatch.mScore match, T.length str, str)
        -- !_ <- FuzzyMatch.computeHeatmap store str seps
        -- pure (0, T.length str, str)

  -- = L.sortOn (\(score, str) -> (Down score, str, T.length str))
  -- . map (\str -> (fm seps needle needleChars str, str))
  -- where
  --   needleChars = FuzzyMatch.prepareNeedle needle


-- {-# NOINLINE doMatchSline #-}
-- doMatchSline :: PrimArray Int32 -> Text -> [Text] -> [(Int, Text)]
-- doMatchSline seps needle
--   = L.sortOn (\(score, str) -> (Down score, str, T.length str))
--   . map (\str -> (fmSline seps needle needleChars str, str))
--   where
--     needleChars = Sline.prepareNeedle needle
--
-- -- {-# NOINLINE fm #-}
-- fmSline :: PrimArray Int32 -> Text -> Sline.NeedleChars -> Text -> Int
-- fmSline seps needle needleChars haystack =
--   Sline.mScore
--     (Sline.fuzzyMatch
--       (Sline.computeHeatmap haystack seps)
--       needle
--       needleChars
--       haystack)

{-# NOINLINE doMatchSline #-}
doMatchSline :: PrimArray Int32 -> Text -> [Text] -> [(Int, Text)]
doMatchSline seps needle
  = L.sortOn (\(score, str) -> (Down score, str, T.length str))
  . map (\str -> (fmSline seps needle needleChars str, str))
  where
    needleChars = Sline.prepareNeedle needle

-- {-# NOINLINE fm #-}
fmSline :: PrimArray Int32 -> Text -> Sline.NeedleChars -> Text -> Int
fmSline seps needle needleChars haystack =
  Sline.mScore
    (Sline.fuzzyMatch
      (Sline.computeHeatmap haystack seps)
      needle
      needleChars
      haystack)




main :: IO ()
main = do
  [n, k] <- getArgs

  let n', k' :: Int
      n' = read n
      k' = read k

  let needle :: Text
      needle = "vector.hs"
      -- seps = primArrayFromList [ord '/']

  candidates <- take k' . T.lines <$> T.readFile "/home/sergey/projects/emacs/projects/emacs-native/candidates.txt"
  evaluate $ rnf candidates
  putStrLn $ "Number of candidates = " ++ show (length candidates)

  -- let !totalScore = sum $ map (\i -> sum $ map fst $ doMatch (primArrayFromList [fromIntegral i]) needle candidates) [1..n']
  -- putStrLn $ "totalScore = " ++ show totalScore
  -- let !kSline = sum $ map (\i -> sum $ map fst $ doMatchSline (primArrayFromList [fromIntegral i]) needle candidates) [1..n']
  -- putStrLn $ "kSline = " ++ show kSline

  let seps :: PrimArray Int32
      seps = primArrayFromList [fromIntegral n']

  -- for_ candidates $ \candidate -> do
  -- -- let candidate = ".emacs.d" :: Text
  -- -- do
  --   let oldHeatmap = primArrayToList (Sline.computeHeatmap candidate seps)
  --   let newHeatmap = runST $ do
  --         store <- FuzzyMatch.mkReusableState (T.length needle) (FuzzyMatch.prepareNeedle needle)
  --         P.toList . FuzzyMatch.unHeatmap <$> FuzzyMatch.computeHeatmap store candidate (T.length candidate) seps
  --   when (map fromIntegral oldHeatmap /= map FuzzyMatch.unHeat newHeatmap) $ do
  --     putStrLn $ "candidate = " ++ show candidate
  --     putStrLn $ "oldHeatmap = " ++ show oldHeatmap
  --     putStrLn $ "newHeatmap = " ++ show newHeatmap
  --     putStrLn ""

  let newMatches = doMatch seps needle candidates
  let oldMatches = doMatchSline seps needle candidates
  for_ (zip3 candidates oldMatches newMatches) $ \(candidate, (oldScore, old), (newScore, new)) -> do
    when (oldScore /= newScore) $ do
      putStrLn $ "oldScore = " ++ show oldScore
      putStrLn $ "newScore = " ++ show newScore
      putStrLn $ "old heatmap = " ++ show (primArrayToList (Sline.computeHeatmap candidate seps))
      let newHeatmap = runST $ do
            store <- FuzzyMatch.mkReusableState (T.length needle) (FuzzyMatch.prepareNeedle needle)
            FuzzyMatch.computeHeatmap store candidate (T.length candidate) seps

      putStrLn $ "new heatmap = " ++ show (P.toList $ FuzzyMatch.unHeatmap newHeatmap)


      putStrLn $ "old = " ++ show old
      putStrLn $ "new = " ++ show new

  -- let seps :: PrimArray Int32
  --     seps = primArrayFromList [fromIntegral $ ord '/']
  -- let old = doMatchSline seps needle candidates
  -- let new = doMatch seps needle candidates
  --
  -- putStrLn $ "Old score: " ++ show (sum $ map fst old)
  -- putStrLn $ "New score: " ++ show (sum $ map fst new)
  --
  -- for_ (L.take n' $ filter (\(x, y) -> x /= y) $ L.zip old new) $ \(old', new') ->
  --   putStrLn $ show old' ++ " | " ++ show new'

  pure ()

