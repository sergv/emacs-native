----------------------------------------------------------------------------
-- |
-- Module      :  FuzzyBench
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}

{-# OPTIONS_GHC -O2 -ddump-simpl -dsuppress-uniques -dsuppress-idinfo -dsuppress-module-prefixes -dsuppress-type-applications -dsuppress-coercions -dppr-cols200 -dsuppress-type-signatures -ddump-to-file #-}

module FuzzyBench (main) where

import Prelude hiding (pi, last)

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Counter qualified as Counter
import Control.Concurrent.Fork
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.Char
import Data.Int
import Data.Ord
import Data.Primitive.PrimArray
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Traversable
import Data.Vector qualified as V
import Data.Vector.Ext qualified as VExt
import Data.Vector.Primitive qualified as P
import Data.Vector.Primitive.Mutable qualified as PM
import Data.Word
import GHC.IO

import Data.FuzzyMatch qualified
import Data.Vector.PredefinedSorts
import Data.Vector.PredefinedSortsPar
import Data.Vector.PredefinedSortsParFake
import Data.Vector.PredefinedSortsParStrategies

import Test.Tasty.Bench

newtype SortKey v = SortKey { _unSortKey :: (Int32, Int, v) }

instance Eq (SortKey v) where
  SortKey (a, b, _) == SortKey (a', b', _) = a == a' && b == b'

instance Ord (SortKey v) where
  SortKey (a, b, _) `compare` SortKey (a', b', _) = Down a `compare` Down a' <> b `compare` b'

{-# INLINE fi32 #-}
fi32 :: Integral a => a -> Int32
fi32 = fromIntegral

-- {-# SPECIALISE VExt.qsort :: Parallel -> PM.MVector RealWorld Word64 -> IO () #-}
--
-- qsortWord64Par :: PM.MVector RealWorld Word64 -> IO ()
-- qsortWord64Par xs = do
--   p@(Parallel pp) <- mkParallel =<< getNumCapabilities
--   VExt.qsort p xs
--   val <- Counter.get pp -- atomically $ readTVar pp
--   when (val > 0) $
--     putStrLn $ "qsort forked " ++ show val ++ " times"


w64 :: Integral a => a -> Word64
w64 = fromIntegral

{-# NOINLINE qsortSeq #-}
qsortSeq :: P.Vector Word64 -> IO (PM.MVector RealWorld Word64)
qsortSeq xs = do
  ys <- P.thaw xs
  stToIO $ qsortWord64 ys
  -- VExt.qsort Sequential ys
  pure ys

{-# NOINLINE qsortPar #-}
qsortPar :: P.Vector Word64 -> IO (PM.MVector RealWorld Word64)
qsortPar xs = do
  ys <- P.thaw xs
  qsortWord64Par ys
  -- Parallel pp <- qsortWord64Par ys
  -- val <- Counter.get pp -- atomically $ readTVar pp
  -- when (val > 0) $
  --   putStrLn $ "qsort forked " ++ show val ++ " times"
  pure ys

{-# NOINLINE qsortParFake #-}
qsortParFake :: P.Vector Word64 -> IO (PM.MVector RealWorld Word64)
qsortParFake xs = do
  ys <- P.thaw xs
  qsortWord64ParFake ys
  -- Parallel pp <- qsortWord64Par ys
  -- val <- Counter.get pp -- atomically $ readTVar pp
  -- when (val > 0) $
  --   putStrLn $ "qsort forked " ++ show val ++ " times"
  pure ys

{-# NOINLINE qsortParStrategies #-}
qsortParStrategies :: P.Vector Word64 -> IO (PM.MVector RealWorld Word64)
qsortParStrategies xs = do
  ys <- P.thaw xs
  qsortWord64ParStrategies ys
  -- Parallel pp <- qsortWord64Par ys
  -- val <- Counter.get pp -- atomically $ readTVar pp
  -- when (val > 0) $
  --   putStrLn $ "qsort forked " ++ show val ++ " times"
  pure ys



incrementTVar :: TVar Int -> Int -> IO ()
incrementTVar v = go
  where
    go 0 = pure ()
    go n = do
      atomically $ modifyTVar' v (+ 1)
      go (n - 1)



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

  let fuzzyMatchOpt :: V.Vector Text -> P.Vector Word64
      fuzzyMatchOpt xs =

        P.convert ((\(x, _, _) -> w64 x) <$> ys)
        -- fmap (\(SortKey (_, _, str)) -> str) $
        --   VExt.sortVectorUnsafe $
        --     (\zs -> coerce zs :: V.Vector (SortKey Text)) ys

        where
          needleChars = Data.FuzzyMatch.prepareNeedle needle
          ys :: V.Vector (Int32, Int, Text)
          ys = runST $ do
            store <- Data.FuzzyMatch.mkReusableState (T.length needle) needleChars
            for xs $ \str -> do
              let !len = T.length str
              !match <- Data.FuzzyMatch.fuzzyMatch' store (Data.FuzzyMatch.computeHeatmap store str len seps32) needle needleChars str
              pure (fi32 $ Data.FuzzyMatch.mScore match, len, str)

  let candidates' :: [P.Vector Word64]
      candidates' =
        map (P.fromList . zipWith (\(n :: Int) (c :: Int) -> (w64 c `unsafeShiftL` 32) .|. w64 n) [0..] . map ord . T.unpack) candidates

  let scores     = fuzzyMatchOpt candidatesV
      scoresHuge = P.concat $ replicate 100 scores

  putStrLn $ "P.length scores = " ++ show (P.length scores)
  putStrLn $ "P.length scoresHuge = " ++ show (P.length scoresHuge)

  v <- newTVarIO 0

  defaultMain
    [ -- bench "Optimized Haskell fuzzy match" $ nf fuzzyMatchOpt candidatesV
    -- bench "Optimized Haskell fuzzy match" $ nf fuzzyMatchOpt candidatesV
      -- bench "Many small Sequential" $ nfAppIO (traverse qsortSeq) candidates'
    -- , bench "Many small Parallel"   $ nfAppIO (traverse qsortPar) candidates'
      bench "Increment TVar 50 times"   $ nfAppIO (incrementTVar v) 50
    , bench "Increment TVar 2000 times" $ nfAppIO (incrementTVar v) 2000

    , bench "One large Sequential"    $ nfAppIO qsortSeq scores
    , bench "One large Parallel"      $ nfAppIO qsortPar scores
    , bench "One large ParallelFake"  $ nfAppIO qsortParFake scores
    , bench "One large ParStrategies" $ nfAppIO qsortParStrategies scores
    , bench "One huge Sequential"     $ nfAppIO qsortSeq scoresHuge
    , bench "One huge Parallel"       $ nfAppIO qsortPar scoresHuge
    , bench "One huge ParallelFake"   $ nfAppIO qsortParFake scoresHuge
    , bench "One huge ParStrategies"  $ nfAppIO qsortParStrategies scoresHuge
    ]
