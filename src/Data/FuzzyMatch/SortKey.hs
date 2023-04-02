----------------------------------------------------------------------------
-- |
-- Module      :  Data.FuzzyMatch.SortKey
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.FuzzyMatch.SortKey
  ( SortKey(..)
  , mkSortKey
  , getScore
  , scoreL
  , lenL
  , idxL
  , signL
  ) where

import Control.LensBlaze
import Data.Coerce
import Data.Int
import Data.Ord
import Data.Primitive.Types
import Data.Word

newtype SortKey = SortKey Word64
  deriving (Prim)

unSortKey :: SortKey -> Word64
unSortKey = coerce

instance Show SortKey where
  show x = "SortKey " ++ show (getScore x, view lenL x, view idxL x)

instance Eq SortKey where
  {-# INLINE (==) #-}
  x == y = getScore x == getScore y && view lenL x == view lenL y

instance Ord SortKey where
  {-# INLINE compare #-}
  x `compare` y =
    Down (getScore x) `compare` Down (getScore y) <> view lenL x `compare` view lenL y
  {-# INLINE (<=) #-}
  x <= y = case compare x y of
    GT -> False
    _  -> True
  {-# INLINE (>=) #-}
  x >= y = case compare x y of
    LT -> False
    _  -> True
  {-# INLINE (<) #-}
  x < y = case compare x y of
    LT -> True
    _  -> False
  {-# INLINE (>) #-}
  x > y = case compare x y of
    GT -> True
    _  -> False

{-# INLINE getScore #-}
getScore :: SortKey -> Int32
getScore x
  | view signL x = view scoreL x
  | otherwise    = negate (view scoreL x)

{-# INLINE sortKeyL #-}
sortKeyL :: Lens' SortKey Word64
sortKeyL = lens unSortKey (\x _ -> SortKey x)

{-# INLINE scoreL #-}
scoreL :: Lens' SortKey Int32
scoreL = sortKeyL . int21L 0

{-# INLINE lenL #-}
lenL :: Lens' SortKey Word32
lenL = sortKeyL . word21L 21

{-# INLINE idxL #-}
idxL :: Lens' SortKey Word32
idxL = sortKeyL . word21L 42

{-# INLINE signL #-}
signL :: Lens' SortKey Bool
signL = sortKeyL . boolL 63

{-# INLINE mkSortKey #-}
mkSortKey :: Int32 -> Word32 -> Word32 -> SortKey
mkSortKey score len idx =
  set scoreL (abs score) $ set lenL len $ set idxL idx $ set signL (score >= 0) $ start
  where
    start :: SortKey
    start = SortKey 0

