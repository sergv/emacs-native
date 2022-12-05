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

import Data.FuzzyMatch qualified

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
    needleChars = Data.FuzzyMatch.prepareNeedle needle

    ys :: V.Vector (Int32, Int, Text)
    ys = runST $ do
      store <- Data.FuzzyMatch.mkReusableState (T.length needle) needleChars
      for (V.fromList xs) $ \str -> do
        match <-
          Data.FuzzyMatch.fuzzyMatch'
            store
            (Data.FuzzyMatch.computeHeatmap store str seps)
            needle
            needleChars
            str
        pure (fi32 $ Data.FuzzyMatch.mScore match, T.length str, str)
        -- !_ <- Data.FuzzyMatch.computeHeatmap store str seps
        -- pure (0, T.length str, str)

  -- = L.sortOn (\(score, str) -> (Down score, str, T.length str))
  -- . map (\str -> (fm seps needle needleChars str, str))
  -- where
  --   needleChars = Data.FuzzyMatch.prepareNeedle needle

-- -- {-# NOINLINE fm #-}
-- fm :: PrimArray Int32 -> Text -> Data.FuzzyMatch.NeedleChars -> Text -> Int
-- fm seps needle needleChars haystack =
--   fromIntegral $
--   Data.FuzzyMatch.mScore
--     (Data.FuzzyMatch.fuzzyMatch
--       (Data.FuzzyMatch.computeHeatmap haystack seps)
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

  pure ()

