----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Ext
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Vector.Ext
  ( binSearchMember
  , linSearchMember
  , binSearchMemberIdx
  , binSearchMemberL
  , uniq
  , sortVectorUnsafe
  , qsort
  , forM
  , primVectorToPrimArray
  ) where

import Prelude hiding (last)

import Control.Concurrent.Fork
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Median
import Data.Primitive.ByteArray
import Data.Primitive.PrimArray
import Data.Vector.FixedSort
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Primitive qualified as P

{-# INLINE forM #-}
forM :: (PrimMonad m, G.Vector v a, G.Vector v b) => v a -> (a -> m b) -> m (v b)
forM !xs f = do
  ys <- GM.unsafeNew end
  let go !i
        | i == end  = G.unsafeFreeze ys
        | otherwise = do
          stToPrim . GM.unsafeWrite ys i =<< f (xs `G.unsafeIndex` i)
          go (i + 1)
  go 0
  where
    !end = G.length xs

{-# INLINE binSearchMember #-}
binSearchMember
  :: (Ord a, G.Vector v a)
  => a
  -> v a
  -> Bool
binSearchMember !a !as =
  binSearchMemberByBounds a as 0 (G.length as)

{-# INLINE linSearchMember #-}
linSearchMember
  :: (Ord a, G.Vector v a)
  => a
  -> v a
  -> Bool
linSearchMember !a !as = go 0
  where
    !end = G.length as
    go !i
      | i == end                  = False
      | as `G.unsafeIndex` i == a = True
      | otherwise                 = go (i + 1)

{-# INLINE binSearchMemberByBounds #-}
binSearchMemberByBounds
  :: (Ord a, G.Vector v a)
  => a
  -> v a
  -> Int
  -> Int
  -> Bool
binSearchMemberByBounds !a !as = go
  where
    go !l !u
      | u <= l    = False
      | otherwise =
        case compare (as `G.unsafeIndex` m) a of
          LT -> go (m + 1) u
          EQ -> True
          GT -> go l m
     where
       m :: Int
       !m = (u + l) `unsafeShiftR` 1

-- {-# INLINE binSearchMemberByBoundsM #-}
-- binSearchMemberByBoundsM
--   :: (PrimMonad m, Ord a, GM.MVector v a)
--   => a
--   -> v (PrimState m) a
--   -> Int
--   -> Int
--   -> m Bool
-- binSearchMemberByBoundsM !a !as = go
--   where
--     go !l !u
--       | u <= l    = pure False
--       | otherwise = do
--         a' <- as `GM.unsafeRead` m
--         case compare a' a of
--           LT -> go (m + 1) u
--           EQ -> pure True
--           GT -> go l m
--      where
--        m :: Int
--        !m = (u + l) `unsafeShiftR` 1

{-# INLINE binSearchMemberIdx #-}
binSearchMemberIdx
  :: (Ord a, G.Vector v a)
  => a
  -> v a
  -> (Bool, Int)
binSearchMemberIdx !x !as =
  binSearchMemberIdxByBounds x as 0 (G.length as)

{-# INLINE binSearchMemberIdxByBounds #-}
binSearchMemberIdxByBounds
  :: (Ord a, G.Vector v a)
  => a
  -> v a
  -> Int
  -> Int
  -> (Bool, Int)
binSearchMemberIdxByBounds !a !as = go
  where
    go !l !u
      | u <= l    = (False, m)
      | otherwise =
        case compare (as `G.unsafeIndex` m) a of
          LT -> go (m + 1) u
          EQ -> (True, m)
          GT -> go l m
     where
       m :: Int
       !m = (u + l) `unsafeShiftR` 1

{-# INLINE binSearchMemberL #-}
binSearchMemberL
  :: (G.Vector v e, Ord e)
  => e
  -> v e
  -> (Bool, Int)
binSearchMemberL !target !as =
  binSearchMemberLByBounds target as 0 (G.length as)

{-# INLINE binSearchMemberLByBounds #-}
binSearchMemberLByBounds
  :: (G.Vector v e, Ord e)
  => e -> v e -> Int -> Int -> (Bool, Int)
binSearchMemberLByBounds !target !as =
  go False
  where
    go found !l !u
      | u <= l    = (found, l)
      | otherwise = do
        case compare (as `G.unsafeIndex` k) target of
          LT -> go found (k + 1) u
          EQ -> go True l k
          GT -> go found l k
      where
        !k = (u + l) `unsafeShiftR` 1

{-# INLINE uniq #-}
uniq :: (Eq a, G.Vector v a) => v a -> v a
uniq !xs
  | G.null xs = G.empty
  | otherwise = runST $ do
    let !end = G.length xs
    ys <- GM.unsafeNew end

    let !firstItem = G.unsafeHead xs
    GM.unsafeWrite ys 0 firstItem

    let go !lastItem !i !j
          | i == end  = pure j
          | otherwise = do
            let !x = xs `G.unsafeIndex` i
            if x == lastItem
            then go lastItem (i + 1) j
            else do
              GM.unsafeWrite ys j x
              go x (i + 1) (j + 1)
    last <- go firstItem 1 1
    G.unsafeFreeze $ GM.unsafeSlice 0 last ys

{-# INLINE sortVectorUnsafe #-}
sortVectorUnsafe :: forall a v. (Ord a, G.Vector v a, Median (Median3 a) a) => v a -> v a
sortVectorUnsafe !xs = runST $ do
  ys <- G.unsafeThaw xs
  qsort Sequential (Median3 @a) ys
  G.unsafeFreeze ys

{-# INLINABLE qsort #-}
qsort
  :: (Fork p x m, Median med a, PrimMonad m, Ord a, GM.MVector v a)
  => p -> med -> v (PrimState m) a -> m ()
qsort = qsortImpl

{-# INLINE qsortImpl #-}
qsortImpl
  :: forall p med x m a v.
     (Fork p x m, Median med a, PrimMonad m, Ord a, GM.MVector v a)
  => p
  -> med
  -> v (PrimState m) a
  -> m ()
qsortImpl !p !med !vector = do
  !releaseToken <- startWork p
  qsortLoop 0 releaseToken threshold vector
  where
    threshold :: Int
    !threshold = binlog2 (GM.length vector)

    qsortLoop :: Int -> x -> Int -> v (PrimState m) a -> m ()
    qsortLoop !depth !releaseToken !cutoff !v
      | len < 17
      = bitonicSort len v *> endWork p releaseToken
      | cutoff == 0
      = heapSort v *> endWork p releaseToken
      | otherwise = do
        let last :: Int
            !last = len - 1
        !pv  <- selectMedian med v
        !pi' <- partitionTwoWays pv last v
        let !left    = GM.unsafeSlice 0 pi' v
            !right   = GM.unsafeSlice pi' (len - pi') v
            !cutoff' = cutoff - 1
            !depth'  = depth + 1
        fork
          p
          releaseToken
          depth
          (\token -> qsortLoop depth' token cutoff')
          (\token -> qsortLoop depth' token cutoff')
          left
          right
        -- qsortLoop cutoff' left
        -- qsortLoop cutoff' right
      where
        !len = GM.length v

{-# INLINE partitionTwoWays #-}
partitionTwoWays
  :: forall m a v. (PrimMonad m, Ord a, GM.MVector v a)
  => a -> Int -> v (PrimState m) a -> m Int
partitionTwoWays !pv !lastIdx !v =
  go 0 lastIdx
  where
    go :: Int -> Int -> m Int
    go !i !j = do
      (i', xi) <- goLT i
      (j', xj) <- goGT j
      if i' < j'
      then do
        GM.unsafeWrite v j' xi
        GM.unsafeWrite v i' xj
        go (i' + 1) (j' - 1)
      else
        pure i'
      where
        goLT !k = do
          x <- GM.unsafeRead v k
          if x < pv && k <= j
          then goLT (k + 1)
          else pure (k, x)
        goGT !k = do
          x <- GM.unsafeRead v k
          if x >= pv && k > i
          then goGT (k - 1)
          else pure (k, x)

{-# INLINE binlog2 #-}
binlog2 :: Int -> Int
binlog2 x = 63 - countLeadingZeros x

{-# INLINE shiftDown #-}
shiftDown :: (PrimMonad m, Ord a, GM.MVector v a) => v (PrimState m) a -> Int -> m ()
shiftDown !v = go
  where
    !end = GM.length v
    go !p
      | c1 < end
      = do
        let !c2 = c1 + 1
        c1Val <- GM.unsafeRead v c1
        (maxIdx, maxVal) <-
          if c2 < end
          then do
            c2Val <- GM.unsafeRead v c2
            pure $ if c1Val > c2Val then (c1, c1Val) else (c2, c2Val)
          else pure (c1, c1Val)
        pVal <- GM.unsafeRead v p
        if maxVal > pVal
        then do
          GM.unsafeWrite v p maxVal
          GM.unsafeWrite v maxIdx pVal
          go maxIdx
        else
          pure ()
      | otherwise
      = pure ()
      where
        !c1 = p * 2 + 1

{-# INLINE heapify #-}
heapify :: (PrimMonad m, Ord a, GM.MVector v a) => v (PrimState m) a -> m ()
heapify !v = do
  go (GM.length v `unsafeShiftR` 1)
  where
    go 0 = shiftDown v 0
    go n = shiftDown v n *> go (n - 1)

{-# INLINE heapSort #-}
heapSort :: (PrimMonad m, Ord a, GM.MVector v a) => v (PrimState m) a -> m ()
heapSort !v = do
  heapify v
  go (GM.length v)
  where
    go 0 = pure ()
    go n = do
      let !k = n - 1
      GM.unsafeSwap v 0 k
      shiftDown (GM.unsafeSlice 0 k v) 0
      go k

{-# INLINE primVectorToPrimArray #-}
primVectorToPrimArray :: P.Vector a -> PrimArray a
primVectorToPrimArray (P.Vector _ _ (ByteArray xs)) = PrimArray xs
