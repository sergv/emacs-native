----------------------------------------------------------------------------
-- |
-- Module      :  FuzzyProf
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module FuzzyProf (main) where

import Control.DeepSeq
import Control.Exception
import Control.LensBlaze
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
import System.Environment

import Data.FuzzyMatch qualified as FuzzyMatch
import Data.FuzzyMatch.SortKey

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
      sortSortKeyPar scores
      P.unsafeFreeze scores

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

  let !totalScore = sum $ map (\i -> sum $ map fst $ doMatch (primArrayFromList [fromIntegral i]) needle candidates) [1..n']
  putStrLn $ "totalScore = " ++ show totalScore

  pure ()
