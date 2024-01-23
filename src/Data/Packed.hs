-- |
-- Module:     Data.Packed
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Packed
  ( PackedStrCharIdxAndStrByteIdx
  , mkPackedStrCharIdxAndStrByteIdx
  , unpackIdxs
  , CharAndIdxs(..)

  , PackedStrCharIdxInLower
  , mkPackedStrCharIdxInLower

  , coerceVectorToPackedStrCharIdxInLower

  -- Type family constructors
  , U.Vector(..)
  , U.MVector(..)
  ) where

import Data.Bits
import Data.Coerce
import Data.Function
import Data.Int
import Data.Primitive.Types
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Base qualified as U
import Data.Word
import Prettyprinter.Generics
import Prettyprinter.Show
import Numeric (showHex)

import Data.StrIdx

newtype PackedStrCharIdxAndStrByteIdx = PackedStrCharIdxAndStrByteIdx { _unPackedStrCharIdxAndStrByteIdx :: Word64 }
  deriving (Eq, Ord, Prim, U.Unbox)

instance Show PackedStrCharIdxAndStrByteIdx where
  show = show . unpackIdxs

instance Pretty PackedStrCharIdxAndStrByteIdx where
  pretty = ppShow

newtype instance U.MVector s PackedStrCharIdxAndStrByteIdx = MV_PackedStrCharIdxAndStrByteIdx (U.MVector s Word64)
newtype instance U.Vector    PackedStrCharIdxAndStrByteIdx = V_PackedStrCharIdxAndStrByteIdx  (U.Vector    Word64)
deriving instance GM.MVector U.MVector PackedStrCharIdxAndStrByteIdx
deriving instance G.Vector   U.Vector  PackedStrCharIdxAndStrByteIdx

mkPackedStrCharIdxAndStrByteIdx :: StrCharIdx Int32 -> StrByteIdx Int32 -> PackedStrCharIdxAndStrByteIdx
mkPackedStrCharIdxAndStrByteIdx (StrCharIdx x) (StrByteIdx y) =
  PackedStrCharIdxAndStrByteIdx $ w64 x .|. (w64 y `unsafeShiftL` 32)

{-# INLINE unpackIdxs #-}
unpackIdxs :: PackedStrCharIdxAndStrByteIdx -> (StrCharIdx Int32, StrByteIdx Int32)
unpackIdxs (PackedStrCharIdxAndStrByteIdx x) =
  ( StrCharIdx (fromIntegral (x .&. lower4Bytes))
  , StrByteIdx (fromIntegral ((x .&. upper4Bytes) `unsafeShiftR` 32))
  )

data CharAndIdxs = CharAndIdxs
  { caiChar :: !Char
  , caiIdxs :: !PackedStrCharIdxAndStrByteIdx
  } deriving (Eq, Ord, Generic)

instance Pretty CharAndIdxs where
  pretty = ppGeneric

instance U.IsoUnbox CharAndIdxs (Char, PackedStrCharIdxAndStrByteIdx) where
  {-# INLINE toURepr   #-}
  {-# INLINE fromURepr #-}
  toURepr (CharAndIdxs a b) = (a, b)
  fromURepr (a, b) = CharAndIdxs a b

newtype instance U.MVector s CharAndIdxs = MV_CharAndIdxs (U.MVector s (Char, PackedStrCharIdxAndStrByteIdx))
newtype instance U.Vector    CharAndIdxs = V_CharAndIdxs  (U.Vector    (Char, PackedStrCharIdxAndStrByteIdx))
deriving via (CharAndIdxs `U.As` (Char, PackedStrCharIdxAndStrByteIdx)) instance GM.MVector U.MVector CharAndIdxs
deriving via (CharAndIdxs `U.As` (Char, PackedStrCharIdxAndStrByteIdx)) instance G.Vector   U.Vector  CharAndIdxs
instance U.Unbox CharAndIdxs

{-# INLINE upper4Bytes #-}
{-# INLINE lower4Bytes #-}
upper4Bytes, lower4Bytes :: Integral a => a
upper4Bytes = 0xFFFFFFFF00000000
lower4Bytes = 0x00000000FFFFFFFF

{-# INLINE w64 #-}
w64 :: Integral a => a -> Word64
w64 = fromIntegral

newtype PackedStrCharIdxInLower = PackedStrCharIdxInLower { unPackedStrCharIdxInLower :: Word64 }
  deriving (Prim, U.Unbox)

instance Eq PackedStrCharIdxInLower where
  (==) = (==) `on` StrCharIdx @Int32 . fromIntegral . (lower4Bytes .&.) . unPackedStrCharIdxInLower

instance Ord PackedStrCharIdxInLower where
  compare = compare `on` StrCharIdx @Int32 . fromIntegral . (lower4Bytes .&.) . unPackedStrCharIdxInLower

instance Show PackedStrCharIdxInLower where
  show (PackedStrCharIdxInLower x) = y ++ "/0x" ++ showHex x []
    where
      y = show $ StrCharIdx @Int32 $ fromIntegral $ x .&. lower4Bytes

newtype instance U.MVector s PackedStrCharIdxInLower = MV_PackedStrCharIdxInLower (U.MVector s Word64)
newtype instance U.Vector    PackedStrCharIdxInLower = V_PackedStrCharIdxInLower  (U.Vector    Word64)
deriving instance GM.MVector U.MVector PackedStrCharIdxInLower
deriving instance G.Vector   U.Vector  PackedStrCharIdxInLower

mkPackedStrCharIdxInLower :: StrCharIdx Int32 -> PackedStrCharIdxInLower
mkPackedStrCharIdxInLower = PackedStrCharIdxInLower . (.&. lower4Bytes) . fromIntegral . unStrCharIdx

coerceVectorToPackedStrCharIdxInLower :: U.Vector PackedStrCharIdxAndStrByteIdx -> U.Vector PackedStrCharIdxInLower
coerceVectorToPackedStrCharIdxInLower = coerce
