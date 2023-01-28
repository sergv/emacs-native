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

import Control.Concurrent.Counter qualified as Counter
import Control.Concurrent.Fork
import Control.DeepSeq
import Control.Exception
import Control.LensBlaze
import Control.Monad
import Control.Monad.ST
import Data.Int
import Data.Primitive.PrimArray
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector qualified as V
import Data.Vector.PredefinedSorts
import Data.Vector.Primitive qualified as P
import Data.Vector.Primitive.Mutable qualified as PM
import Data.Word
import System.Environment
import System.IO.Unsafe

import Data.FuzzyMatch qualified as FuzzyMatch
import Data.FuzzyMatch.SortKey

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

{-# NOINLINE qsortPar #-}
qsortPar :: P.Vector Word64 -> IO (P.Vector Word64)
qsortPar xs = do
  ys <- P.thaw xs
  _ <- qsortWord64Par ys
  -- Parallel pp <- qsortWord64Par ys
  -- val <- Counter.get pp -- atomically $ readTVar pp
  -- when (val > 0) $
  --   putStrLn $ "qsort forked " ++ show val ++ " times"
  P.unsafeFreeze ys


main :: IO ()
main = do
  [n, k] <- getArgs

  let n', k' :: Int
      n' = read n
      k' = read k

  let needle :: Text
      needle = "e" -- "vector.hs"
      -- seps = primArrayFromList [ord '/']

  candidates <- take k' . T.lines <$> T.readFile "/home/sergey/projects/emacs/projects/emacs-native/candidates.txt"
  evaluate $ rnf candidates
  putStrLn $ "Number of candidates = " ++ show (length candidates)

  let scoress :: [P.Vector Word64]
      scoress = map (\i -> P.fromList $ map (fromIntegral . fst) $ doMatch (primArrayFromList [fromIntegral i]) needle candidates) [1..n']

  scoress' <- qsortPar $ P.concat $ concat $ replicate 25 scoress
  putStrLn $ "totalScore = " ++ show (P.sum scoress')


  -- let !totalScore = map (\i -> map fst $ doMatch (primArrayFromList [fromIntegral i]) needle candidates) [1..n']
  -- putStrLn $ "totalScore = " ++ show totalScore

  pure ()

