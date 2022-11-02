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
import Data.Int
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
doMatch :: PrimArray Int32 -> Text -> [Text] -> [(Int, Text)]
doMatch seps needle
  = map (\str -> (fm seps needle needleChars str, str))
  where
    needleChars = Data.FuzzyMatch.prepareNeedle needle

-- {-# NOINLINE fm #-}
fm :: PrimArray Int32 -> Text -> Data.FuzzyMatch.NeedleChars -> Text -> Int
fm seps needle needleChars haystack =
  fromIntegral $
  Data.FuzzyMatch.mScore
    (Data.FuzzyMatch.fuzzyMatch
      (Data.FuzzyMatch.computeHeatMap haystack seps)
      needle
      needleChars
      haystack)

{-# NOINLINE doMatchSline #-}
doMatchSline :: Int -> PrimArray Int -> Text -> [Text] -> [(Int, Text)]
doMatchSline _ seps needle
  = L.sortOn (\(score, str) -> (Down score, T.length str))
  . map (\str -> (fmSline seps needle needleChars str, str))
  where
    needleChars = (Sline.prepareNeedle needle)

-- {-# NOINLINE fm #-}
fmSline :: PrimArray Int -> Text -> Sline.NeedleChars -> Text -> Int
fmSline seps needle needleChars haystack =
  Sline.mScore
    (Sline.fuzzyMatch
      (Sline.computeHeatMap haystack seps)
      needle
      needleChars
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

  let !kSline = sum $ map (\i -> sum $ map fst $ doMatchSline i (primArrayFromList [i]) needle candidates) [0..read n]
  putStrLn $ "kSline = " ++ show kSline
  -- let !k = sum $ map (\i -> sum $ map fst $ doMatch (primArrayFromList [i]) needle candidates) [1..read n]
  -- putStrLn $ "k = " ++ show k

  pure ()

