----------------------------------------------------------------------------
-- |
-- Module      :  Data.FuzzyMatch.SortKey
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE TypeFamilies #-}

module Data.FuzzyMatch.SortKey
  ( SortKey
  , mkSortKey
  , getScore
  , scoreL
  , lenL
  , idxL
  ) where

import Control.LensBlaze
import Data.Bits
import Data.Bits.Ext
import Data.Ord
import Data.Primitive.Types
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Base qualified as U
import Data.Word
import Prettyprinter.Generics
import Prettyprinter.Show

newtype PackedLenAndIdx = PackedLenAndIdx { _unPackedLenAndIdx :: Word64 }
  deriving (Eq, Ord, Prim, U.Unbox)

instance Show PackedLenAndIdx where
  show = show . unpackLenAndIdx

instance Pretty PackedLenAndIdx where
  pretty = ppShow

newtype instance U.MVector s PackedLenAndIdx = MV_PackedLenAndIdx (U.MVector s Word64)
newtype instance U.Vector    PackedLenAndIdx = V_PackedLenAndIdx  (U.Vector    Word64)
deriving instance GM.MVector U.MVector PackedLenAndIdx
deriving instance G.Vector   U.Vector  PackedLenAndIdx

mkPackedLenAndIdx :: Word32 -> Word32 -> PackedLenAndIdx
mkPackedLenAndIdx len idx =
  PackedLenAndIdx $ w64 len .|. (w64 idx `unsafeShiftL` 32)

{-# INLINE lenL' #-}
lenL' :: Lens' PackedLenAndIdx Word32
lenL' = coerceL . word32L @Word64 0

{-# INLINE idxL' #-}
idxL' :: Lens' PackedLenAndIdx Word32
idxL' = coerceL . word32L @Word64 32

{-# INLINE unpackLenAndIdx #-}
unpackLenAndIdx :: PackedLenAndIdx -> (Word32, Word32)
unpackLenAndIdx x =
  ( view lenL' x
  , view idxL' x
  )

data SortKey = SortKey
  { skScore     :: !Int
  , skLenAndIdx :: !PackedLenAndIdx
  }

instance Show SortKey where
  show x = "SortKey " ++ show (skScore x, view lenL x, view idxL x)

instance Eq SortKey where
  {-# INLINE (==) #-}
  x == y = skScore x == skScore y && view lenL x == view lenL y

instance Ord SortKey where
  {-# INLINE compare #-}
  x `compare` y =
    Down (skScore x) `compare` Down (skScore y) <> view lenL x `compare` view lenL y
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

instance U.IsoUnbox SortKey (Int, PackedLenAndIdx) where
  {-# INLINE toURepr   #-}
  {-# INLINE fromURepr #-}
  toURepr (SortKey a b) = (a, b)
  fromURepr (a, b) = SortKey a b

newtype instance U.MVector s SortKey = MV_SortKey (U.MVector s (Int, PackedLenAndIdx))
newtype instance U.Vector    SortKey = V_SortKey  (U.Vector    (Int, PackedLenAndIdx))
deriving via (SortKey `U.As` (Int, PackedLenAndIdx)) instance GM.MVector U.MVector SortKey
deriving via (SortKey `U.As` (Int, PackedLenAndIdx)) instance G.Vector   U.Vector  SortKey
instance U.Unbox SortKey

{-# INLINE getScore #-}
getScore :: SortKey -> Int
getScore = skScore

lenAndIdxL :: Lens' SortKey PackedLenAndIdx
lenAndIdxL = lens skLenAndIdx (\a x -> x { skLenAndIdx = a })

{-# INLINE scoreL #-}
scoreL :: Lens' SortKey Int
scoreL = lens skScore (\a x -> x { skScore = a })

{-# INLINE lenL #-}
lenL :: Lens' SortKey Word32
lenL = lenAndIdxL . lenL'

{-# INLINE idxL #-}
idxL :: Lens' SortKey Word32
idxL = lenAndIdxL . idxL'

{-# INLINE mkSortKey #-}
mkSortKey :: Int -> Word32 -> Word32 -> SortKey
mkSortKey score len idx = SortKey
  { skScore     = score
  , skLenAndIdx = mkPackedLenAndIdx len idx
  }
