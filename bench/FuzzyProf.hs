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
import Control.Monad.ST
import Data.Coerce
import Data.Int
import Data.Ord
import Data.Primitive.PrimArray
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Traversable
import Data.Vector qualified as V
import Data.Vector.Ext qualified as VExt
import System.Environment

import Data.FuzzyMatch qualified as FuzzyMatch

-- import Data.List qualified as L
-- import Data.FuzzyMatchBaseline qualified as Sline

newtype SortKey v = SortKey { _unSortKey :: (Int32, Int, v) }

instance Eq (SortKey v) where
  SortKey (a, b, _) == SortKey (a', b', _) = a == a' && b == b'

instance Ord (SortKey v) where
  SortKey (a, b, _) `compare` SortKey (a', b', _) = Down a `compare` Down a' <> b `compare` b'

{-# INLINE fi32 #-}
fi32 :: Integral a => a -> Int32
fi32 = fromIntegral


{-# NOINLINE doMatch #-}
doMatch :: PrimArray Int32 -> Text -> [Text] -> [(Int, Text)]
doMatch seps needle xs =
  V.toList $
    fmap (\(SortKey (score, _, str)) -> (fromIntegral score, str)) $
      VExt.sortVectorUnsafe $
        (\zs -> coerce zs :: V.Vector (SortKey Text)) ys
  where
    needleChars = FuzzyMatch.prepareNeedle needle

    ys :: V.Vector (Int32, Int, Text)
    ys = runST $ do
      store <- FuzzyMatch.mkReusableState (T.length needle) needleChars
      for (V.fromList xs) $ \str -> do
        match <-
          FuzzyMatch.fuzzyMatch'
            store
            (FuzzyMatch.computeHeatmap store str seps)
            needle
            needleChars
            str
        pure (fi32 $ FuzzyMatch.mScore match, T.length str, str)
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



main :: IO ()
main = do
  [n] <- getArgs

  let n' :: Int
      n' = read n

  let needle :: Text
      needle = "e" -- "vector.hs"
      -- seps = primArrayFromList [ord '/']

  candidates <- T.lines <$> T.readFile "/home/sergey/projects/emacs/projects/emacs-native/candidates.txt"
  evaluate $ rnf candidates
  putStrLn $ "Number of candidates = " ++ show (length candidates)

  let !k = sum $ map (\i -> sum $ map fst $ doMatch (primArrayFromList [fromIntegral i]) needle candidates) [1..n']
  putStrLn $ "k = " ++ show k
  -- let !kSline = sum $ map (\i -> sum $ map fst $ doMatchSline (primArrayFromList [fromIntegral i]) needle candidates) [1..n']
  -- putStrLn $ "kSline = " ++ show kSline

  -- let seps :: PrimArray Int32
  --     seps = primArrayFromList [fromIntegral n']
  -- for_ candidates $ \candidate -> do
  -- -- let candidate = ".emacs.d" :: Text
  -- -- do
  --   let oldHeatmap = primArrayToList (Sline.computeHeatmap candidate seps)
  --   let newHeatmap = runST $ do
  --         store <- FuzzyMatch.mkReusableState (T.length needle) (FuzzyMatch.prepareNeedle needle)
  --         P.toList . FuzzyMatch.unHeatmap <$> FuzzyMatch.computeHeatmap store candidate seps
  --   when (map fromIntegral oldHeatmap /= map FuzzyMatch.unHeat newHeatmap) $ do
  --     putStrLn $ "candidate = " ++ show candidate
  --     putStrLn $ "oldHeatmap = " ++ show oldHeatmap
  --     putStrLn $ "newHeatmap = " ++ show newHeatmap
  --     putStrLn ""

  -- let newMatches = doMatch seps needle candidates
  -- let oldMatches = doMatchSline seps needle candidates
  -- for_ (zip3 candidates oldMatches newMatches) $ \(candidate, (oldScore, old), (newScore, new)) -> do
  --   when (oldScore /= newScore) $ do
  --     putStrLn $ "oldScore = " ++ show oldScore
  --     putStrLn $ "newScore = " ++ show newScore
  --     putStrLn $ "old heatmap = " ++ show (primArrayToList (Sline.computeHeatmap candidate seps))
  --     let newHeatmap = runST $ do
  --           store <- FuzzyMatch.mkReusableState (T.length needle) (FuzzyMatch.prepareNeedle needle)
  --           FuzzyMatch.computeHeatmap store candidate seps
  --
  --     putStrLn $ "new heatmap = " ++ show (P.toList $ FuzzyMatch.unHeatmap newHeatmap)
  --
  --
  --     putStrLn $ "old = " ++ show old
  --     putStrLn $ "new = " ++ show new

  pure ()

