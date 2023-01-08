----------------------------------------------------------------------------
-- |
-- Module      :  Data.Primitive.PrimArray.Ext
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Primitive.PrimArray.Ext
  ( binSearchMember
  , primToByteArr
  ) where

import Data.Primitive.ByteArray
import Data.Primitive.PrimArray
import Data.Primitive.Types

{-# INLINE binSearchMember #-}
binSearchMember
  :: (Ord e, Prim e)
  => PrimArray e
  -> e
  -> Bool
binSearchMember xs x = binarySearchMemberByBounds xs x 0 (sizeofPrimArray xs)

{-# INLINE binarySearchMemberByBounds #-}
binarySearchMemberByBounds
  :: (Ord e, Prim e)
  => PrimArray e
  -> e
  -> Int
  -> Int
  -> Bool
binarySearchMemberByBounds xs e = go
  where
    go !l !u
      | u <= l    = False
      | otherwise =
        case compare (indexPrimArray xs m) e of
          LT -> go (m + 1) u
          EQ -> True
          GT -> go l m
     where
       m = (u + l) `div` 2

{-# INLINE primToByteArr #-}
primToByteArr :: MutablePrimArray s a -> MutableByteArray s
primToByteArr (MutablePrimArray arr) = MutableByteArray arr
