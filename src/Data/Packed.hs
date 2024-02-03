-- |
-- Module:     Data.Packed
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Packed
  ( PackedCharAndStrCharIdx
  , mkPackedCharAndStrCharIdx

  , PackedCharInUpper
  , mkPackedCharInUpper
  , coerceVectorToPackedCharInUpper

  , PackedStrCharIdxInLower
  , mkPackedStrCharIdxInLower
  , getStrCharIdx
  , coerceVectorToPackedStrCharIdxInLower

  -- Type family constructors
  , U.Vector(..)
  , U.MVector(..)
  ) where

import Data.Bits
import Data.Bits.Ext
import Data.Char
import Data.Coerce
import Data.Function
import Data.Int
import Data.Primitive.Types
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Base qualified as U
import Data.Word
import Numeric (showHex)
import Prettyprinter.Generics

import Data.StrIdx

--------------------------------------------------------------------------------

newtype PackedCharAndStrCharIdx = PackedCharAndStrCharIdx { _unPackedCharAndStrCharIdx :: Word64 }
  deriving (Eq, Ord, Prim, U.Unbox)

newtype instance U.MVector s PackedCharAndStrCharIdx = MV_PackedCharAndStrCharIdx (U.MVector s Word64)
newtype instance U.Vector    PackedCharAndStrCharIdx = V_PackedCharAndStrCharIdx  (U.Vector    Word64)
deriving instance GM.MVector U.MVector PackedCharAndStrCharIdx
deriving instance G.Vector   U.Vector  PackedCharAndStrCharIdx

mkPackedCharAndStrCharIdx :: Char -> StrCharIdx Int32 -> PackedCharAndStrCharIdx
mkPackedCharAndStrCharIdx char (StrCharIdx idx) =
  PackedCharAndStrCharIdx $ (w64 (ord char) `unsafeShiftL` 32) .|. w64 idx

--------------------------------------------------------------------------------

newtype PackedCharInUpper = PackedCharInUpper { unPackedCharInUpper :: Word64 }
  deriving (U.Unbox)

instance Show PackedCharInUpper where
  showsPrec _ x
    = showString "0x"
    . showHex c
    . showChar '/'
    . showChar (chr (fromIntegral c))
    where
      c :: Word64
      c = keepChar x `unsafeShiftR` 32

instance Pretty PackedCharInUpper where
  pretty = pretty . show

newtype instance U.MVector s PackedCharInUpper = MV_PackedCharInUpper (U.MVector s PackedCharAndStrCharIdx)
newtype instance U.Vector    PackedCharInUpper = V_PackedCharInUpper  (U.Vector    PackedCharAndStrCharIdx)
deriving instance GM.MVector U.MVector PackedCharInUpper
deriving instance G.Vector   U.Vector  PackedCharInUpper

mkPackedCharInUpper :: Char -> PackedCharInUpper
mkPackedCharInUpper = PackedCharInUpper . (`unsafeShiftL` 32) . w64 . ord

keepChar :: PackedCharInUpper -> Word64
keepChar =
  (.&. upper4Bytes) . unPackedCharInUpper

instance Eq PackedCharInUpper where
  (==) = (==) `on` keepChar

instance Ord PackedCharInUpper where
  compare = compare `on` keepChar

coerceVectorToPackedCharInUpper :: U.Vector PackedCharAndStrCharIdx -> U.Vector PackedCharInUpper
coerceVectorToPackedCharInUpper = coerce

--------------------------------------------------------------------------------

newtype PackedStrCharIdxInLower = PackedStrCharIdxInLower { unPackedStrCharIdxInLower :: Word64 }
  deriving (Prim, U.Unbox)

instance Eq PackedStrCharIdxInLower where
  (==) = (==) `on` getStrCharIdx

instance Ord PackedStrCharIdxInLower where
  compare = compare `on` getStrCharIdx

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

{-# INLINE getStrCharIdx #-}
getStrCharIdx :: PackedStrCharIdxInLower -> StrCharIdx Int32
getStrCharIdx = StrCharIdx @Int32 . fromIntegral . (lower4Bytes .&.) . unPackedStrCharIdxInLower

coerceVectorToPackedStrCharIdxInLower :: U.Vector PackedCharAndStrCharIdx -> U.Vector PackedStrCharIdxInLower
coerceVectorToPackedStrCharIdxInLower = coerce
