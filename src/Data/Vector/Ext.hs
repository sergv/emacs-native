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
  , forM
  , primVectorToPrimArray
  ) where

import Prelude hiding (last)

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Primitive.ByteArray
import Data.Primitive.PrimArray
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

{-# INLINE primVectorToPrimArray #-}
primVectorToPrimArray :: P.Vector a -> PrimArray a
primVectorToPrimArray (P.Vector _ _ (ByteArray xs)) = PrimArray xs
