----------------------------------------------------------------------------
-- |
-- Module      :  FuzzyBench
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImportQualifiedPost      #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module FuzzyBench (main) where

import System.Exit
import System.Environment
import GHC.IO

import Prelude hiding (pi, last)

import Control.DeepSeq
import Control.Monad.ST
import Data.Bits
import Data.Char
import Data.Int
import Data.List qualified as L
import Data.Ord
import Data.Primitive.PrimArray
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
-- import System.Environment

import Data.Text.Internal.Unsafe.Char qualified as TUC
import Data.Text.Array qualified as TA
import Data.Text.Internal qualified as TI
import Data.Text.Internal.Encoding.Utf8 qualified as TU8

import Data.FuzzyMatch qualified
import Data.FuzzyMatchBaseline qualified as Sline

import Data.Vector.Growable qualified as VG

import Test.Tasty.Bench

{-# NOINLINE doMatch #-}
doMatch :: Int -> PrimArray Int -> Text -> [Text] -> [(Int, Text)]
doMatch _ seps needle
  = L.sortOn (\(score, str) -> (Down score, T.length str))
  . map (\str -> (fm seps needle str, str))

-- {-# NOINLINE fm #-}
fm :: PrimArray Int -> Text -> Text -> Int
fm seps needle haystack =
  Data.FuzzyMatch.mScore
    (Data.FuzzyMatch.fuzzyMatch
      (Data.FuzzyMatch.computeHeatMap haystack seps)
      needle
      haystack)

{-# NOINLINE doMatchSline #-}
doMatchSline :: Int -> PrimArray Int -> Text -> [Text] -> [(Int, Text)]
doMatchSline _ seps needle
  = L.sortOn (\(score, str) -> (Down score, T.length str))
  . map (\str -> (fmSline seps needle str, str))

-- {-# NOINLINE fm #-}
fmSline :: PrimArray Int -> Text -> Text -> Int
fmSline seps needle haystack =
  Sline.mScore
    (Sline.fuzzyMatch
      (Sline.computeHeatMap haystack seps)
      needle
      haystack)




{-# NOINLINE mkHaystackVector #-}
mkHaystackVector :: [Text] -> [U.Vector Int64]
mkHaystackVector
  = map
    ( U.concatMap (\(idx, c) -> let !c' = toLower c in if c == c' then U.singleton (combineCharIdx c idx) else U.fromList [combineCharIdx c idx, combineCharIdx c' idx])
    . U.indexed
    . U.fromList
    . T.unpack
    )

{-# NOINLINE mkHaystackGrowableVectorUnpackText #-}
mkHaystackGrowableVectorUnpackText :: [Text] -> [U.Vector Int64]
mkHaystackGrowableVectorUnpackText strs = flip map strs $ \str -> runST $ do
  store <- VG.new (T.length str)
  go store 0 (T.unpack str)
  where
    go :: VG.GrowableVector (UM.MVector s Int64) -> Int -> String -> ST s (U.Vector Int64)
    go acc _ []        = VG.unsafeFreeze acc
    go acc !i (c : cs) = do
      let !c' = toLower c
      acc' <-
        if c == c'
        then do
          VG.push (combineCharIdx c i) acc
        else do
          VG.push (combineCharIdx c i) acc >>= VG.push (combineCharIdx c' i)
      go acc' (i + 1) cs





{-# NOINLINE mkHaystackGrowableVectorIterateTextManually #-}
mkHaystackGrowableVectorIterateTextManually :: [Text] -> [U.Vector Int64]
mkHaystackGrowableVectorIterateTextManually = map mkHaystackGrowableVectorIterateTextManually1

mkHaystackGrowableVectorIterateTextManually1 :: Text -> U.Vector Int64
mkHaystackGrowableVectorIterateTextManually1 (TI.Text arr off len) = runST $ do
  store <- VG.new (len + len `unsafeShiftR` 2)
  go store 0 off
  where
    !end = off + len

    go :: VG.GrowableVector (UM.MVector s Int64) -> Int -> Int -> ST s (U.Vector Int64)
    -- go acc _  _ []       = VG.unsafeFreeze acc
    go acc !i j
      | j >= end  = VG.unsafeFreeze acc
      | otherwise = do
        let !n0 = TA.unsafeIndex arr i
            !l = TU8.utf8LengthByLeader n0
            !c = case l of
              1 -> TUC.unsafeChr8 n0
              2 -> TU8.chr2 n0 (TA.unsafeIndex arr (i + 1))
              3 -> TU8.chr3 n0 (TA.unsafeIndex arr (i + 1)) (TA.unsafeIndex arr (i + 2))
              _ -> TU8.chr4 n0 (TA.unsafeIndex arr (i + 1)) (TA.unsafeIndex arr (i + 2)) (TA.unsafeIndex arr (i + 3))

            !c' = toLower c
        acc' <-
          if c == c'
          then do
            VG.push (combineCharIdx c i) acc
          else do
            VG.push (combineCharIdx c i) acc >>= VG.push (combineCharIdx c' i)
        go acc' (i + 1) (j + l)


-- stream (Text arr off len) = Stream next off (betweenSize (len `shiftR` 2) len)
--     where
--       !end = off+len
--       next !i
--           | i >= end  = Done
--           | otherwise = Yield chr (i + l)
--           where
--             n0 = TA.unsafeIndex arr i
--             n1 = TA.unsafeIndex arr (i + 1)
--             n2 = TA.unsafeIndex arr (i + 2)
--             n3 = TA.unsafeIndex arr (i + 3)
--
--             l  = TU8.utf8LengthByLeader n0
--             chr = case l of
--               1 -> unsafeChr8 n0
--               2 -> TU8.chr2 n0 n1
--               3 -> TU8.chr3 n0 n1 n2
--               _ -> TU8.chr4 n0 n1 n2 n3




{-# NOINLINE mkHaystackGrowableVectorIterateTextManuallyReuse #-}
mkHaystackGrowableVectorIterateTextManuallyReuse :: [Text] -> [U.Vector Int64]
mkHaystackGrowableVectorIterateTextManuallyReuse strs = runST $ do
  store <- VG.new 16
  traverse (mkHaystackGrowableVectorIterateTextManuallyReuse1 store) strs

mkHaystackGrowableVectorIterateTextManuallyReuse1
  :: forall s. VG.GrowableVector (UM.MVector s Int64)
  -> Text
  -> ST s (U.Vector Int64)
mkHaystackGrowableVectorIterateTextManuallyReuse1 store (TI.Text arr off len) = go store 0 off
  where
    !end = off + len

    go :: VG.GrowableVector (UM.MVector s Int64) -> Int -> Int -> ST s (U.Vector Int64)
    -- go acc _  _ []       = VG.unsafeFreeze acc
    go acc !i j
      | j >= end  = VG.freeze acc
      | otherwise = do
        let !n0 = TA.unsafeIndex arr i
            !l = TU8.utf8LengthByLeader n0
            !c = case l of
              1 -> TUC.unsafeChr8 n0
              2 -> TU8.chr2 n0 (TA.unsafeIndex arr (i + 1))
              3 -> TU8.chr3 n0 (TA.unsafeIndex arr (i + 1)) (TA.unsafeIndex arr (i + 2))
              _ -> TU8.chr4 n0 (TA.unsafeIndex arr (i + 1)) (TA.unsafeIndex arr (i + 2)) (TA.unsafeIndex arr (i + 3))

            !c' = toLower c
        acc' <-
          if c == c'
          then do
            VG.push (combineCharIdx c i) acc
          else do
            VG.push (combineCharIdx c i) acc >>= VG.push (combineCharIdx c' i)
        go acc' (i + 1) (j + l)




fi64 :: Integral a => a -> Int64
fi64 = fromIntegral

combineCharIdx :: Char -> Int -> Int64
combineCharIdx c idx = (fi64 (ord c) `unsafeShiftL` 32) .|. fi64 idx

{-# NOINLINE mkHaystackList #-}
mkHaystackList :: [Text] -> [U.Vector Int64]
mkHaystackList
  = map
    ( U.fromList
    . concatMap (\(idx, c) -> let !c' = toLower c in if c == c' then [combineCharIdx c idx] else [combineCharIdx c idx, combineCharIdx c' idx])
    . zip [0..]
    . T.unpack
    )


main :: IO ()
main = do
  [n] <- getArgs

  let needle :: Text
      needle = "vector.hs"
      seps = primArrayFromList [ord '/']

  candidates <- T.lines <$> T.readFile "/home/sergey/projects/emacs/projects/emacs-native/candidates.txt"
  evaluate $ rnf candidates
  putStrLn $ "Number of candidates = " ++ show (length candidates)

  let !kSline = sum $ map (\i -> sum $ map fst $ doMatchSline i seps needle candidates) [0..read n]
  putStrLn $ "kSline = " ++ show kSline
  let !k = sum $ map (\i -> sum $ map fst $ doMatch i seps needle candidates) [0..read n]
  putStrLn $ "k = " ++ show k

  _ <- die "We're done"

  let origScore str = Sline.mScore $ Sline.fuzzyMatch (Sline.computeHeatMap str seps) needle str

      optScore str = Data.FuzzyMatch.mScore $ Data.FuzzyMatch.fuzzyMatch (Data.FuzzyMatch.computeHeatMap str seps) needle str

      fuzzyMatch getScore
        = L.sortOn (\(score, str) -> (Down score, T.length str))
        . map (\str -> (getScore str, str))

  defaultMain
    [ bench "mkHaystackList"                                   $ nf mkHaystackList candidates
    , bench "mkHaystackVector"                                 $ nf mkHaystackVector candidates
    , bench "mkHaystackGrowableVectorUnpackText"               $ nf mkHaystackGrowableVectorUnpackText candidates
    , bench "mkHaystackGrowableVectorIterateTextManually"      $ nf mkHaystackGrowableVectorIterateTextManually candidates
    , bench "mkHaystackGrowableVectorIterateTextManuallyReuse" $ nf mkHaystackGrowableVectorIterateTextManuallyReuse candidates

    , bench "Original Haskell fuzzy match"  $ nf (fuzzyMatch origScore) candidates
    , bench "Optimized Haskell fuzzy match" $ nf (fuzzyMatch optScore) candidates
    ]
