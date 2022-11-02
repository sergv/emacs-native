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
import Data.Primitive.PrimArray.Ext qualified as PExt
import Data.Primitive.PrimArray.Growable qualified as PG
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Ext qualified as T
import Data.Text.IO qualified as T
import Data.Vector.Ext qualified as VExt
import Data.Vector.Primitive qualified as P
import Data.Vector.Primitive.Mutable qualified as PM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

import Data.Text.Array qualified as TA
import Data.Text.Internal qualified as TI
import Data.Text.Internal.Encoding.Utf8 qualified as TU8
import Data.Text.Internal.Unsafe.Char qualified as TUC
import Data.Text.Unsafe qualified as TU

import Data.FuzzyMatch qualified
import Data.FuzzyMatchBaseline qualified as Sline

import Data.Vector.Growable qualified as VG

import Test.Tasty.Bench

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



{-# NOINLINE mkHaystackGrowableVectorIterateTextWithIter #-}
mkHaystackGrowableVectorIterateTextWithIter :: [Text] -> [U.Vector Int64]
mkHaystackGrowableVectorIterateTextWithIter = map mkHaystackGrowableVectorIterateTextWithIter1

mkHaystackGrowableVectorIterateTextWithIter1 :: Text -> U.Vector Int64
mkHaystackGrowableVectorIterateTextWithIter1 (TI.Text arr off len) = runST $ do
  store <- VG.new (len + len `unsafeShiftR` 2)
  go store 0 off
  where
    !end = off + len

    go :: VG.GrowableVector (UM.MVector s Int64) -> Int -> Int -> ST s (U.Vector Int64)
    go acc !i j
      | j >= end  = VG.unsafeFreeze acc
      | otherwise = do
        let TU.Iter c delta = TU.iterArray arr j
            !c'             = toLower c
        acc' <-
          if c == c'
          then do
            VG.push (combineCharIdx c i) acc
          else do
            VG.push (combineCharIdx c i) acc >>= VG.push (combineCharIdx c' i)
        go acc' (i + 1) (j + delta)

{-# NOINLINE mkHaystackGrowableVectorIterateTextWithTextFold #-}
mkHaystackGrowableVectorIterateTextWithTextFold :: [Text] -> [U.Vector Int64]
mkHaystackGrowableVectorIterateTextWithTextFold = map mkHaystackGrowableVectorIterateTextWithTextFold1

mkHaystackGrowableVectorIterateTextWithTextFold1 :: Text -> U.Vector Int64
mkHaystackGrowableVectorIterateTextWithTextFold1 str = runST $ do
  store <- VG.new (len + len `unsafeShiftR` 2)
  T.textFoldM go (0, store) str >>= VG.unsafeFreeze . snd
  where
    !len = T.length str

    go
      :: Char
      -> (Int, VG.GrowableVector (UM.MVector s Int64))
      -> ST s (Int, VG.GrowableVector (UM.MVector s Int64))
    go !c (!i, !acc) = do
      let !c' = toLower c
      acc' <-
        if c == c'
        then do
          VG.push (combineCharIdx c i) acc
        else do
          VG.push (combineCharIdx c i) acc >>= VG.push (combineCharIdx c' i)
      pure (i + 1, acc')


newtype NeedleChars = NeedleChars { unNeedleChars :: P.Vector Char }

prepareNeedle :: Text -> NeedleChars
prepareNeedle str
  = NeedleChars
  $ VExt.sortVectorUnsafe
  $ T.textToPrimVector
  $ T.toLower str <> T.toUpper str

needleMember :: Char -> NeedleChars -> Bool
needleMember c = VExt.binSearchMember c . unNeedleChars

{-# NOINLINE mkHaystackNeedleChars #-}
mkHaystackNeedleChars :: NeedleChars -> [Text] -> [P.Vector Int64]
mkHaystackNeedleChars needleChars =
  map (\xs -> runST (P.unsafeFreeze =<< mkHaystackNeedleChars1 needleChars xs))

mkHaystackNeedleChars1 :: forall s. NeedleChars -> Text -> ST s (PM.MVector s Int64)
mkHaystackNeedleChars1 needleChars str = do
  store  <- PG.new (len + (len `unsafeShiftR` 2))
  arr    <- PG.finalise =<< T.textFoldIdxM go store str
  arrLen <- getSizeofMutablePrimArray arr
  pure $ P.MVector 0 arrLen $ PExt.primToByteArr arr
  where
    !len = T.length str

    go
      :: Int
      -> Char
      -> PG.GrowablePrimArray s Int64
      -> ST s (PG.GrowablePrimArray s Int64)
    go !i !c !acc
      | needleMember c needleChars = do
        let !c' = toLower c
        if c == c'
        then do
          PG.push (combineCharIdx c i) acc
        else do
          PG.push (combineCharIdx c i) acc >>= PG.push (combineCharIdx c' i)
      | otherwise =
        pure acc






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
  -- [n] <- getArgs

  let needle :: Text
      needle = "vector.hs"
      seps32 :: PrimArray Int32
      seps32 = primArrayFromList [fromIntegral $ ord '/']

  candidates <- T.lines <$> T.readFile "/home/sergey/projects/emacs/projects/emacs-native/candidates.txt"
  evaluate $ rnf candidates
  putStrLn $ "Number of candidates = " ++ show (length candidates)

  -- let !kSline = sum $ map (\i -> sum $ map fst $ doMatchSline i seps needle candidates) [0..read n]
  -- putStrLn $ "kSline = " ++ show kSline
  -- let !k = sum $ map (\i -> sum $ map fst $ doMatch i seps needle candidates) [0..read n]
  -- putStrLn $ "k = " ++ show k
  --
  -- _ <- die "We're done"

  let origScore str = Sline.mScore $ Sline.fuzzyMatch (Sline.computeHeatMap str seps32) needle (Sline.prepareNeedle needle) str

      optScore str = Data.FuzzyMatch.mScore $ Data.FuzzyMatch.fuzzyMatch (Data.FuzzyMatch.computeHeatMap str seps32) needle (Data.FuzzyMatch.prepareNeedle needle) str

      fuzzyMatch getScore
        = L.sortOn (\(score, str) -> (Down score, T.length str))
        . map (\str -> (getScore str, str))

  defaultMain
    [ bench "mkHaystackList"                                   $ nf mkHaystackList candidates
    , bench "mkHaystackVector"                                 $ nf mkHaystackVector candidates
    , bench "mkHaystackGrowableVectorUnpackText"               $ nf mkHaystackGrowableVectorUnpackText candidates
    , bench "mkHaystackGrowableVectorIterateTextManually"      $ nf mkHaystackGrowableVectorIterateTextManually candidates
    , bench "mkHaystackGrowableVectorIterateTextWithIter"      $ nf mkHaystackGrowableVectorIterateTextWithIter candidates
    , bench "mkHaystackGrowableVectorIterateTextManuallyReuse" $ nf mkHaystackGrowableVectorIterateTextManuallyReuse candidates
    , bench "mkHaystackGrowableVectorIterateTextWithTextFold"  $ nf mkHaystackGrowableVectorIterateTextWithTextFold candidates
    , bench "mkHaystackNeedleChars"                            $ nf (mkHaystackNeedleChars (prepareNeedle needle)) candidates

    , bench "Original Haskell fuzzy match"  $ nf (fuzzyMatch origScore) candidates
    , bench "Optimized Haskell fuzzy match" $ nf (fuzzyMatch optScore) candidates
    ]
