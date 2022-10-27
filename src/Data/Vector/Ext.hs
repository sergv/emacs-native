----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Ext
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Vector.Ext
  ( binSearchMember
  , binSearchMemberIdx
  , binSearchMemberL
  , qsort
  ) where

import Prelude hiding (last)

import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM

{-# INLINE binSearchMember #-}
binSearchMember
  :: (Ord a, G.Vector v a)
  => a
  -> v a
  -> Bool
binSearchMember a as =
  binSearchMemberByBounds a as 0 (G.length as)

{-# INLINE binSearchMemberByBounds #-}
binSearchMemberByBounds
  :: (Ord a, G.Vector v a)
  => a
  -> v a
  -> Int
  -> Int
  -> Bool
binSearchMemberByBounds a as = go
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
       !m = (u + l) `div` 2


{-# INLINE binSearchMemberIdx #-}
binSearchMemberIdx
  :: (Ord a, G.Vector v a)
  => a
  -> v a
  -> (Bool, Int)
binSearchMemberIdx x as =
  binSearchMemberIdxByBounds x as 0 (G.length as)

{-# INLINE binSearchMemberIdxByBounds #-}
binSearchMemberIdxByBounds
  :: (Ord a, G.Vector v a)
  => a
  -> v a
  -> Int
  -> Int
  -> (Bool, Int)
binSearchMemberIdxByBounds a as = go
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
       !m = (u + l) `div` 2

{-# INLINE binSearchMemberL #-}
binSearchMemberL
  :: (G.Vector v e, Ord e)
  => e
  -> v e
  -> (Bool, Int)
binSearchMemberL target as =
  binSearchMemberLByBounds target as 0 (G.length as)

{-# INLINE binSearchMemberLByBounds #-}
binSearchMemberLByBounds
  :: (G.Vector v e, Ord e)
  => e -> v e -> Int -> Int -> (Bool, Int)
binSearchMemberLByBounds target as =
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
        k = (u + l) `div` 2

{-# INLINE qsort #-}
qsort :: (PrimMonad m, Ord a, GM.MVector v a) => v (PrimState m) a -> m ()
qsort vector = go vector threshold
  where
    threshold = binlog2 (GM.length vector)
    go v !cutoff
      | len < 17
      = bitonicSort len v
      | cutoff == 0
      = heapSort v
      | otherwise = do
        let pi0, pi1, pi2 :: Int
            !pi0  = 0
            !pi1  = len `unsafeShiftR` 1
            !last = len - 1
            !pi2  = last
        pv0 <- GM.unsafeRead v pi0
        pv1 <- GM.unsafeRead v pi1
        pv2 <- GM.unsafeRead v pi2
        pv  <-
          if pv0 > pv1
          then
            -- ... p1 < p0 ...
            if pv0 > pv2
            then
              if pv1 > pv2
              then do
                -- p2 < p1 < p0
                GM.unsafeWrite v pi1 pv2
                GM.unsafeWrite v pi2 pv1
                pure pv1
              else do
                -- p1 <= p2 < p0
                pure pv2
            else do
              --  p1 < p0 <= p2
              GM.unsafeWrite v pi0 pv2
              GM.unsafeWrite v pi2 pv0
              pure pv0
          else
            -- ... p0 <= p1 ...
            if pv1 > pv2
            then
              if pv0 > pv2
              then do
                -- p2 < p0 <= p1
                GM.unsafeWrite v pi0 pv2
                GM.unsafeWrite v pi2 pv0
                pure pv0
              else do
                -- p0 <= p2 <= p1
                pure pv2
            else do
              -- p0 <= p1 <= p2
              GM.unsafeWrite v pi1 pv2
              GM.unsafeWrite v pi2 pv1
              pure pv1
        pi' <- partitionTwoWays pv last v
        let !pi''    = pi' + 1
            !left    = GM.unsafeSlice 0 pi' v
            !right   = GM.unsafeSlice pi'' (len - pi'') v
            !cutoff' = cutoff - 1
        go left cutoff'
        go right cutoff'
      where
        len = GM.length v

{-# INLINE partitionTwoWays #-}
partitionTwoWays :: (PrimMonad m, Ord a, GM.MVector v a) => a -> Int -> v (PrimState m) a -> m Int
partitionTwoWays !pv !lastIdx !v =
  go 0 (lastIdx - 1)
  where
    go !i !j = do
      (i', xi) <- goLT i
      (j', xj) <- goGT j
      if i' < j'
      then do
        GM.unsafeWrite v j' xi
        GM.unsafeWrite v i' xj
        go (i' + 1) (j' - 1)
      else do
        GM.unsafeSwap v i' lastIdx
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
shiftDown v = go
  where
    !end = GM.length v
    go p
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
heapify v = do
  go (GM.length v `div` 2)
  where
    go 0 = shiftDown v 0
    go n = shiftDown v n *> go (n - 1)

{-# INLINE heapSort #-}
heapSort :: (PrimMonad m, Ord a, GM.MVector v a) => v (PrimState m) a -> m ()
heapSort v = do
  heapify v
  go (GM.length v)
  where
    go 0 = pure ()
    go n = do
      let !k = n - 1
      GM.unsafeSwap v 0 k
      shiftDown (GM.unsafeSlice 0 k v) 0
      go k

{-# INLINABLE sort3 #-}
-- | Sorts the elements at the three given indices. The indices are assumed
-- to be given from lowest to highest, so if 'l < m < u' then
-- 'sort3ByIndex cmp a m l u' essentially sorts the median of three into the
-- lowest position in the array.
sort3
  :: (PrimMonad m, GM.MVector v a, Ord a)
  => v (PrimState m) a -> m ()
sort3 xs = do
  a0 <- GM.unsafeRead xs 0
  x1 <- GM.unsafeRead xs 1
  x2 <- GM.unsafeRead xs 2
  if a0 > x1
  then
    if a0 > x2
    then
      if x2 < x1
      then do
        GM.unsafeWrite xs 0 x2
        GM.unsafeWrite xs 2 a0
      else do
         GM.unsafeWrite xs 0 x1
         GM.unsafeWrite xs 1 x2
         GM.unsafeWrite xs 2 a0
    else do
      GM.unsafeWrite xs 0 x1
      GM.unsafeWrite xs 1 a0
  else
    if x1 > x2
    then
      if a0 > x2
      then do
        GM.unsafeWrite xs 0 x2
        GM.unsafeWrite xs 1 a0
        GM.unsafeWrite xs 2 x1
      else do
        GM.unsafeWrite xs 1 x2
        GM.unsafeWrite xs 2 x1
    else
      pure ()

{-# INLINABLE sort4 #-}
-- | Sorts the elements at the four given indices. Like the 2 and 3 element
-- versions, this assumes that the indices are given in increasing order, so
-- it can be used to sort medians into particular positions and so on.
sort4
  :: (PrimMonad m, GM.MVector v a, Ord a)
  => v (PrimState m) a -> m ()
sort4 xs = do
  a0 <- GM.unsafeRead xs 0
  x1 <- GM.unsafeRead xs 1
  x2 <- GM.unsafeRead xs 2
  x3 <- GM.unsafeRead xs 3
  if a0 > x1
  then
    if a0 > x2
    then
      if x1 > x2
      then
        if x1 > x3
        then
          if x2 > x3
          then do
            GM.unsafeWrite xs 0 x3
            GM.unsafeWrite xs 1 x2
            GM.unsafeWrite xs 2 x1
            GM.unsafeWrite xs 3 a0
          else do
            GM.unsafeWrite xs 0 x2
            GM.unsafeWrite xs 1 x3
            GM.unsafeWrite xs 2 x1
            GM.unsafeWrite xs 3 a0
        else
          if a0 > x3
          then do
            GM.unsafeWrite xs 0 x2
            GM.unsafeWrite xs 1 x1
            GM.unsafeWrite xs 2 x3
            GM.unsafeWrite xs 3 a0
          else do
            GM.unsafeWrite xs 0 x2
            GM.unsafeWrite xs 1 x1
            GM.unsafeWrite xs 2 a0
            GM.unsafeWrite xs 3 x3
      else
        if x2 > x3
        then
          if x1 > x3
          then do
            GM.unsafeWrite xs 0 x3
            GM.unsafeWrite xs 1 x1
            GM.unsafeWrite xs 2 x2
            GM.unsafeWrite xs 3 a0
          else do
            GM.unsafeWrite xs 0 x1
            GM.unsafeWrite xs 1 x3
            GM.unsafeWrite xs 2 x2
            GM.unsafeWrite xs 3 a0
        else
          if a0 > x3
          then do
            GM.unsafeWrite xs 0 x1
            GM.unsafeWrite xs 1 x2
            GM.unsafeWrite xs 2 x3
            GM.unsafeWrite xs 3 a0
          else do
            GM.unsafeWrite xs 0 x1
            GM.unsafeWrite xs 1 x2
            GM.unsafeWrite xs 2 a0
            -- GM.unsafeWrite xs 3 x3
    else
      if a0 > x3
      then
        if x1 > x3
        then do
          GM.unsafeWrite xs 0 x3
          -- GM.unsafeWrite xs 1 x1
          GM.unsafeWrite xs 2 a0
          GM.unsafeWrite xs 3 x2
        else do
          GM.unsafeWrite xs 0 x1
          GM.unsafeWrite xs 1 x3
          GM.unsafeWrite xs 2 a0
          GM.unsafeWrite xs 3 x2
      else
        if x2 > x3
        then do
          GM.unsafeWrite xs 0 x1
          GM.unsafeWrite xs 1 a0
          GM.unsafeWrite xs 2 x3
          GM.unsafeWrite xs 3 x2
        else do
          GM.unsafeWrite xs 0 x1
          GM.unsafeWrite xs 1 a0
          -- GM.unsafeWrite xs 2 x2
          -- GM.unsafeWrite xs 3 x3
  else
    if x1 > x2
    then
      if a0 > x2
      then
        if a0 > x3
        then
          if x2 > x3
          then do
            GM.unsafeWrite xs 0 x3
            GM.unsafeWrite xs 1 x2
            GM.unsafeWrite xs 2 a0
            GM.unsafeWrite xs 3 x1
          else do
            GM.unsafeWrite xs 0 x2
            GM.unsafeWrite xs 1 x3
            GM.unsafeWrite xs 2 a0
            GM.unsafeWrite xs 3 x1
        else
          if x1 > x3
          then do
            GM.unsafeWrite xs 0 x2
            GM.unsafeWrite xs 1 a0
            GM.unsafeWrite xs 2 x3
            GM.unsafeWrite xs 3 x1
          else do
            GM.unsafeWrite xs 0 x2
            GM.unsafeWrite xs 1 a0
            GM.unsafeWrite xs 2 x1
            -- GM.unsafeWrite xs 3 x3
      else
        if x2 > x3
        then
          if a0 > x3
          then do
            GM.unsafeWrite xs 0 x3
            GM.unsafeWrite xs 1 a0
            -- GM.unsafeWrite xs 2 x2
            GM.unsafeWrite xs 3 x1
          else do
            -- GM.unsafeWrite xs 0 a0
            GM.unsafeWrite xs 1 x3
            -- GM.unsafeWrite xs 2 x2
            GM.unsafeWrite xs 3 x1
        else
          if x1 > x3
          then do
            -- GM.unsafeWrite xs 0 a0
            GM.unsafeWrite xs 1 x2
            GM.unsafeWrite xs 2 x3
            GM.unsafeWrite xs 3 x1
          else do
            -- GM.unsafeWrite xs 0 a0
            GM.unsafeWrite xs 1 x2
            GM.unsafeWrite xs 2 x1
            -- GM.unsafeWrite xs 3 x3
    else
      if x1 > x3
      then
        if a0 > x3
        then do
          GM.unsafeWrite xs 0 x3
          GM.unsafeWrite xs 1 a0
          GM.unsafeWrite xs 2 x1
          GM.unsafeWrite xs 3 x2
        else do
          -- GM.unsafeWrite xs 0 a0
          GM.unsafeWrite xs 1 x3
          GM.unsafeWrite xs 2 x1
          GM.unsafeWrite xs 3 x2
      else
        if x2 > x3
        then do
          -- GM.unsafeWrite xs 0 a0
          -- GM.unsafeWrite xs 1 x1
          GM.unsafeWrite xs 2 x3
          GM.unsafeWrite xs 3 x2
        else do
          -- GM.unsafeWrite xs 0 a0
          -- GM.unsafeWrite xs 1 x1
          -- GM.unsafeWrite xs 2 x2
          -- GM.unsafeWrite xs 3 x3
          pure ()

{-# INLINABLE bitonicSort #-}
bitonicSort :: forall m v a. (PrimMonad m, Ord a, GM.MVector v a) => Int -> v (PrimState m) a -> m ()
bitonicSort n v = do
  case n of
    2  ->
      swap 0 1
    3  ->
      -- swap 1 2
      -- swap 0 2
      -- swap 0 1
      sort3 v
    4  ->
      -- swap 0 1
      -- swap 2 3
      -- swap 0 2
      -- swap 1 3
      -- swap 1 2
      sort4 v
    5  -> do
      swap 0 1
      swap 3 4
      swap 2 4
      swap 2 3
      swap 1 4
      swap 0 3
      swap 0 2
      swap 1 3
      swap 1 2
    6  -> do
      swap 1 2
      swap 4 5
      swap 0 2
      swap 3 5
      swap 0 1
      swap 3 4
      swap 2 5
      swap 0 3
      swap 1 4
      swap 2 4
      swap 1 3
      swap 2 3
    7  -> do
      swap 1 2
      swap 3 4
      swap 5 6
      swap 0 2
      swap 3 5
      swap 4 6
      swap 0 1
      swap 4 5
      swap 2 6
      swap 0 4
      swap 1 5
      swap 0 3
      swap 2 5
      swap 1 3
      swap 2 4
      swap 2 3
    8  -> do
      swap 0 1
      swap 2 3
      swap 4 5
      swap 6 7
      swap 0 2
      swap 1 3
      swap 4 6
      swap 5 7
      swap 1 2
      swap 5 6
      swap 0 4
      swap 3 7
      swap 1 5
      swap 2 6
      swap 1 4
      swap 3 6
      swap 2 4
      swap 3 5
      swap 3 4
    9  -> do
      swap 0 1
      swap 3 4
      swap 6 7
      swap 1 2
      swap 4 5
      swap 7 8
      swap 0 1
      swap 3 4
      swap 6 7
      swap 2 5
      swap 0 3
      swap 1 4
      swap 5 8
      swap 3 6
      swap 4 7
      swap 2 5
      swap 0 3
      swap 1 4
      swap 5 7
      swap 2 6
      swap 1 3
      swap 4 6
      swap 2 4
      swap 5 6
      swap 2 3
    10 -> do
      swap 4 9
      swap 3 8
      swap 2 7
      swap 1 6
      swap 0 5
      swap 1 4
      swap 6 9
      swap 0 3
      swap 5 8
      swap 0 2
      swap 3 6
      swap 7 9
      swap 0 1
      swap 2 4
      swap 5 7
      swap 8 9
      swap 1 2
      swap 4 6
      swap 7 8
      swap 3 5
      swap 2 5
      swap 6 8
      swap 1 3
      swap 4 7
      swap 2 3
      swap 6 7
      swap 3 4
      swap 5 6
      swap 4 5
    11 -> do
      swap 0 1
      swap 2 3
      swap 4 5
      swap 6 7
      swap 8 9
      swap 1 3
      swap 5 7
      swap 0 2
      swap 4 6
      swap 8 10
      swap 1 2
      swap 5 6
      swap 9 10
      swap 0 4
      swap 3 7
      swap 1 5
      swap 6 10
      swap 4 8
      swap 5 9
      swap 2 6
      swap 0 4
      swap 3 8
      swap 1 5
      swap 6 10
      swap 2 3
      swap 8 9
      swap 1 4
      swap 7 10
      swap 3 5
      swap 6 8
      swap 2 4
      swap 7 9
      swap 5 6
      swap 3 4
      swap 7 8
    12 -> do
      swap 0 1
      swap 2 3
      swap 4 5
      swap 6 7
      swap 8 9
      swap 10 11
      swap 1 3
      swap 5 7
      swap 9 11
      swap 0 2
      swap 4 6
      swap 8 10
      swap 1 2
      swap 5 6
      swap 9 10
      swap 0 4
      swap 7 11
      swap 1 5
      swap 6 10
      swap 3 7
      swap 4 8
      swap 5 9
      swap 2 6
      swap 0 4
      swap 7 11
      swap 3 8
      swap 1 5
      swap 6 10
      swap 2 3
      swap 8 9
      swap 1 4
      swap 7 10
      swap 3 5
      swap 6 8
      swap 2 4
      swap 7 9
      swap 5 6
      swap 3 4
      swap 7 8
    13 -> do
      swap 1 7
      swap 9 11
      swap 3 4
      swap 5 8
      swap 0 12
      swap 2 6
      swap 0 1
      swap 2 3
      swap 4 6
      swap 8 11
      swap 7 12
      swap 5 9
      swap 0 2
      swap 3 7
      swap 10 11
      swap 1 4
      swap 6 12
      swap 7 8
      swap 11 12
      swap 4 9
      swap 6 10
      swap 3 4
      swap 5 6
      swap 8 9
      swap 10 11
      swap 1 7
      swap 2 6
      swap 9 11
      swap 1 3
      swap 4 7
      swap 8 10
      swap 0 5
      swap 2 5
      swap 6 8
      swap 9 10
      swap 1 2
      swap 3 5
      swap 7 8
      swap 4 6
      swap 2 3
      swap 4 5
      swap 6 7
      swap 8 9
      swap 3 4
      swap 5 6
    14 -> do
      swap 0 1
      swap 2 3
      swap 4 5
      swap 6 7
      swap 8 9
      swap 10 11
      swap 12 13
      swap 0 2
      swap 4 6
      swap 8 10
      swap 1 3
      swap 5 7
      swap 9 11
      swap 0 4
      swap 8 12
      swap 1 5
      swap 9 13
      swap 2 6
      swap 3 7
      swap 0 8
      swap 1 9
      swap 2 10
      swap 3 11
      swap 4 12
      swap 5 13
      swap 5 10
      swap 6 9
      swap 3 12
      swap 7 11
      swap 1 2
      swap 4 8
      swap 1 4
      swap 7 13
      swap 2 8
      swap 5 6
      swap 9 10
      swap 2 4
      swap 11 13
      swap 3 8
      swap 7 12
      swap 6 8
      swap 10 12
      swap 3 5
      swap 7 9
      swap 3 4
      swap 5 6
      swap 7 8
      swap 9 10
      swap 11 12
      swap 6 7
      swap 8 9
    15 -> do
      swap 0 1
      swap 2 3
      swap 4 5
      swap 6 7
      swap 8 9
      swap 10 11
      swap 12 13
      swap 0 2
      swap 4 6
      swap 8 10
      swap 12 14
      swap 1 3
      swap 5 7
      swap 9 11
      swap 0 4
      swap 8 12
      swap 1 5
      swap 9 13
      swap 2 6
      swap 10 14
      swap 3 7
      swap 0 8
      swap 1 9
      swap 2 10
      swap 3 11
      swap 4 12
      swap 5 13
      swap 6 14
      swap 5 10
      swap 6 9
      swap 3 12
      swap 13 14
      swap 7 11
      swap 1 2
      swap 4 8
      swap 1 4
      swap 7 13
      swap 2 8
      swap 11 14
      swap 5 6
      swap 9 10
      swap 2 4
      swap 11 13
      swap 3 8
      swap 7 12
      swap 6 8
      swap 10 12
      swap 3 5
      swap 7 9
      swap 3 4
      swap 5 6
      swap 7 8
      swap 9 10
      swap 11 12
      swap 6 7
      swap 8 9
    16 -> do
      swap 0 1
      swap 2 3
      swap 4 5
      swap 6 7
      swap 8 9
      swap 10 11
      swap 12 13
      swap 14 15
      swap 0 2
      swap 4 6
      swap 8 10
      swap 12 14
      swap 1 3
      swap 5 7
      swap 9 11
      swap 13 15
      swap 0 4
      swap 8 12
      swap 1 5
      swap 9 13
      swap 2 6
      swap 10 14
      swap 3 7
      swap 11 15
      swap 0 8
      swap 1 9
      swap 2 10
      swap 3 11
      swap 4 12
      swap 5 13
      swap 6 14
      swap 7 15
      swap 5 10
      swap 6 9
      swap 3 12
      swap 13 14
      swap 7 11
      swap 1 2
      swap 4 8
      swap 1 4
      swap 7 13
      swap 2 8
      swap 11 14
      swap 5 6
      swap 9 10
      swap 2 4
      swap 11 13
      swap 3 8
      swap 7 12
      swap 6 8
      swap 10 12
      swap 3 5
      swap 7 9
      swap 3 4
      swap 5 6
      swap 7 8
      swap 9 10
      swap 11 12
      swap 6 7
      swap 8 9
    _ ->
      pure ()
  where
    swap :: Int -> Int -> m ()
    swap i j = do
      x <- GM.unsafeRead v i
      y <- GM.unsafeRead v j
      when (x > y) $ do
        GM.unsafeWrite v i y
        GM.unsafeWrite v j x


