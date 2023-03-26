----------------------------------------------------------------------------
-- |
-- Module      :  FuzzyProf
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module FuzzyProf (main) where

-- import Control.Concurrent.Counter qualified as Counter
-- import Control.Concurrent.Fork
-- import Control.Monad

import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception
import Control.LensBlaze
import Control.Monad.ST
import Data.ByteString qualified as BS
import Data.Int
import Data.List qualified as L
import Data.Primitive.PrimArray
import Data.Set (Set)
import Data.Set qualified as S
import Data.Store qualified as Store
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector qualified as V
import Data.Vector.PredefinedSorts
import Data.Vector.PredefinedSortsPar
import Data.Vector.PredefinedSortsParStrategies
import Data.Vector.Primitive qualified as P
import Data.Vector.Primitive.Mutable qualified as PM
import Data.Word
import Debug.Trace (traceEventIO)
import System.Clock.Seconds
import System.IO.Unsafe
import System.Random.Stateful
import Text.Printf

import Data.FuzzyMatch qualified as FuzzyMatch
import Data.FuzzyMatch.SortKey

import GHC.Conc.Sync (labelThread, myThreadId)

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
    ys = unsafePerformIO $ do
      scores' <- stToIO $ do

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
        pure scores
      qsortSortKeyPar scores'
      P.unsafeFreeze scores'

{-# NOINLINE qsortParStrat #-}
qsortParStrat :: P.Vector Word64 -> IO (P.Vector Word64)
qsortParStrat xs = do
  ys <- P.thaw xs
  _ <- qsortWord64ParStrategies ys
  -- Parallel pp <- qsortWord64Par ys
  -- val <- Counter.get pp -- atomically $ readTVar pp
  -- when (val > 0) $
  --   putStrLn $ "qsort forked " ++ show val ++ " times"
  P.unsafeFreeze ys

{-# NOINLINE qsortSeq #-}
qsortSeq :: P.Vector Word64 -> IO (P.Vector Word64)
qsortSeq xs = do
  ys <- P.thaw xs
  _ <- stToIO $ qsortWord64 ys
  -- Parallel pp <- qsortWord64Par ys
  -- val <- Counter.get pp -- atomically $ readTVar pp
  -- when (val > 0) $
  --   putStrLn $ "qsort forked " ++ show val ++ " times"
  P.unsafeFreeze ys



main :: IO ()
main = do
  -- [n, k] <- getArgs
  --
  -- let n', k' :: Int
  --     n' = read n
  --     k' = read k

  -- let needle :: Text
  --     needle = "e" -- "vector.hs"
  --     -- seps = primArrayFromList [ord '/']

  -- candidates <- T.lines <$> T.readFile "/home/sergey/projects/emacs/projects/emacs-native/candidates.txt"
  -- evaluate $ rnf candidates
  -- putStrLn $ "Number of candidates = " ++ show (length candidates)
  --
  -- let xs :: P.Vector Word64
  --     xs = P.fromList $ map (fromIntegral . fst) $ doMatch (primArrayFromList [fromIntegral 1]) needle candidates
  --
  -- BS.writeFile "/home/sergey/projects/emacs/projects/emacs-native/candidates.store" $ Store.encode xs
  (xs :: P.Vector Word64) <-
    Store.decodeIO =<< BS.readFile "/home/sergey/projects/emacs/projects/emacs-native/candidates.store"

  -- let xs :: P.Vector Int32
  --     xs = P.map fromIntegral xs

  --
  -- let scoress :: [P.Vector Word64]
  --     scoress = map (\i -> P.fromList $ map (fromIntegral . fst) $ doMatch (primArrayFromList [fromIntegral i]) needle candidates) [1..n']

  --
  -- putStrLn $ "length = " ++ show (length $ concat $ map P.toList scoress)
  -- scoress' <- qsortParStrat $ P.concat $ concat $ replicate 25 scoress
  -- putStrLn $ "totalScore = " ++ show (P.sum scoress')

  tid <- myThreadId
  labelThread tid "Main thread"

  -- let len :: Int
  --     -- len = 200000 -- 1000000
  --     len = 50000 -- 1000000
  --
  --     gen = mkStdGen 42
  --
  --     -- xs :: P.Vector Word64
  --     -- -- xs = P.replicateM len $ randomRM (0, 2 * len)
  --     -- xs =
  --     --   runSTGen_ gen $ \g' ->
  --     --     P.replicateM len $ uniformRM (0, 2 * fromIntegral len) g'

  putStrLn $ "Size = " ++ show (P.length xs)
  putStrLn $ "xs =\n" ++ show (L.sort (ordNub (map (\x -> fromIntegral x :: Int) (P.toList xs))))
  putStrLn $ "xs =\n" ++ show (map (\x -> fromIntegral x :: Int) (P.toList xs))
  evaluate $ rnf xs

  concurrently_
    (event' "Sort Seq" $ qsortSeq xs)
    (event' "Sort ParStrat" $ qsortParStrat xs)
  -- _ <- qsortParStrat $ P.generate len (\n -> fromIntegral $! len - n)

  -- let !totalScore = map (\i -> map fst $ doMatch (primArrayFromList [fromIntegral i]) needle candidates) [1..n']
  -- putStrLn $ "totalScore = " ++ show totalScore

  pure ()

event' :: String -> IO a -> IO a
-- event' _label = id
event' label action = do
  traceEventIO $ "START " ++ label
  start <- getTime Monotonic
  !res <- action
  end <- getTime Monotonic
  traceEventIO $ "STOP "  ++ label
  putStrLn $ printf "%s took %f s" (leftpad 16 label) (fromIntegral (toNanoSecs end - toNanoSecs start) / fromIntegral 1_000_000_000 :: Double)
  pure res

leftpad :: Int -> String -> String
leftpad n str = str ++ replicate (n - length str) ' '

ordNub :: Ord a => [a] -> [a]
ordNub = go mempty
  where
    go !acc = \case
      [] -> []
      x : xs
        | S.member x acc -> go acc xs
        | otherwise      -> x : go (S.insert x acc) xs

