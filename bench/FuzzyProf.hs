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
import Data.Char
import Data.Foldable
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
  = L.sortOn (\(score, str) -> (Down score, str, T.length str))
  . map (\str -> (fm seps needle needleChars str, str))
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
      (Sline.computeHeatMap haystack seps)
      needle
      needleChars
      haystack)



main :: IO ()
main = do
  [n] <- getArgs

  let n' :: Int
      n' = read n

  let needle :: Text
      needle = "vector.hs"
      -- seps = primArrayFromList [ord '/']

  candidates <- T.lines <$> T.readFile "/home/sergey/projects/emacs/projects/emacs-native/candidates.txt"
  evaluate $ rnf candidates
  putStrLn $ "Number of candidates = " ++ show (length candidates)

  -- let !kSline = sum $ map (\i -> sum $ map fst $ doMatchSline (primArrayFromList [fromIntegral i]) needle candidates) [1..n']
  -- putStrLn $ "kSline = " ++ show kSline
  let !k = sum $ map (\i -> sum $ map fst $ doMatch (primArrayFromList [fromIntegral i]) needle candidates) [1..n']
  putStrLn $ "k = " ++ show k

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

