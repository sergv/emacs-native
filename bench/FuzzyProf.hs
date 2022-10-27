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

module FuzzyProf (main) where

import Control.DeepSeq
import Control.Exception
import Data.List qualified as L
import Data.Ord
import Data.Primitive.PrimArray
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Environment

import Data.FuzzyMatch qualified
import Data.FuzzyMatchBaseline qualified as Sline

{-# NOINLINE doMatch #-}
doMatch :: PrimArray Int -> Text -> [Text] -> [(Int, Text)]
doMatch seps needle
  = map (\str -> (fm seps needle str, str))

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



main :: IO ()
main = do
  [n] <- getArgs

  let needle :: Text
      needle = "vector.hs"
      -- seps = primArrayFromList [ord '/']

  candidates <- T.lines <$> T.readFile "/home/sergey/projects/emacs/projects/emacs-native/candidates.txt"
  evaluate $ rnf candidates
  putStrLn $ "Number of candidates = " ++ show (length candidates)

  -- let !kSline = sum $ map (\i -> sum $ map fst $ doMatchSline i seps needle candidates) [0..read n]
  -- putStrLn $ "kSline = " ++ show kSline
  let !k = sum $ map (\i -> sum $ map fst $ doMatch (primArrayFromList [i]) needle candidates) [1..read n]
  putStrLn $ "k = " ++ show k

  pure ()

