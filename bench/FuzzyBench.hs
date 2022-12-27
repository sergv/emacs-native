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
import Data.Char
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

import Data.FuzzyMatch qualified

import Test.Tasty.Bench

newtype SortKey v = SortKey { _unSortKey :: (Int32, Int, v) }

instance Eq (SortKey v) where
  SortKey (a, b, _) == SortKey (a', b', _) = a == a' && b == b'

instance Ord (SortKey v) where
  SortKey (a, b, _) `compare` SortKey (a', b', _) = Down a `compare` Down a' <> b `compare` b'

{-# INLINE fi32 #-}
fi32 :: Integral a => a -> Int32
fi32 = fromIntegral


main :: IO ()
main = do
  -- [n] <- getArgs

  let needle :: Text
      needle = "vector.hs"
      seps32 :: PrimArray Int32
      seps32 = primArrayFromList [fromIntegral $ ord '/']

  candidates <- T.lines <$> T.readFile "/home/sergey/projects/emacs/projects/emacs-native/candidates.txt"
  let candidatesV = V.fromList candidates
  evaluate $ rnf candidates
  putStrLn $ "Number of candidates = " ++ show (length candidates)

  -- let !kSline = sum $ map (\i -> sum $ map fst $ doMatchSline i seps needle candidates) [0..read n]
  -- putStrLn $ "kSline = " ++ show kSline
  -- let !k = sum $ map (\i -> sum $ map fst $ doMatch i seps needle candidates) [0..read n]
  -- putStrLn $ "k = " ++ show k
  --
  -- _ <- die "We're done"

  let fuzzyMatchOpt xs =

        fmap (\(SortKey (_, _, str)) -> str) $
          VExt.sortVectorUnsafe $
            (\zs -> coerce zs :: V.Vector (SortKey Text)) ys

        where
          needleChars = Data.FuzzyMatch.prepareNeedle needle
          ys :: V.Vector (Int32, Int, Text)
          ys = runST $ do
            store <- Data.FuzzyMatch.mkReusableState (T.length needle) needleChars
            for xs $ \str -> do
              let !len = T.length str
              !match <- Data.FuzzyMatch.fuzzyMatch' store (Data.FuzzyMatch.computeHeatmap store str len seps32) needle needleChars str
              pure (fi32 $ Data.FuzzyMatch.mScore match, len, str)


  defaultMain
    [ bench "Optimized Haskell fuzzy match" $ nf fuzzyMatchOpt candidatesV
    ]
