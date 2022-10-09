----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Algorithms.Search.Ext
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Data.Vector.Algorithms.Search.Ext (binSearchMember) where

import Data.Vector.Unboxed qualified as U

{-# INLINE binSearchMember #-}
binSearchMember
  :: (Ord e, U.Unbox e)
  => U.Vector e
  -> e
  -> Bool
binSearchMember xs x = binarySearchMemberByBounds xs x 0 (U.length xs)

{-# INLINE binarySearchMemberByBounds #-}
binarySearchMemberByBounds
  :: (Ord e, U.Unbox e)
  => U.Vector e
  -> e
  -> Int
  -> Int
  -> Bool
binarySearchMemberByBounds xs e = go
  where
    go !l !u
      | u <= l    = False
      | otherwise =
        case compare (U.unsafeIndex xs m) e of
          LT -> go (m + 1) u
          EQ -> True
          GT -> go l m
     where
       m = (u + l) `div` 2
