-- |
-- Module:     Data.Packed
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE TypeFamilies #-}

module Data.Packed
  ( PackedCharAndIdx(..)
  , combineCharIdx
  , PackedChar(..)
  , mkPackedChar
  , keepChar
  , PackedIdx(..)
  , keepIdx
  , mkPackedIdx
  , getStrIdx

  , U.Vector(..)
  , U.MVector(..)
  ) where

import Data.Bits
import Data.Char
import Data.Function
import Data.Primitive.Types
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Base qualified as U
import Data.Word
import Numeric (showHex)
import Prettyprinter
import Prettyprinter.Show

import Data.StrIdx

newtype PackedCharAndIdx = PackedCharAndIdx { _unPackedCharAndIdx :: Word64 }
  deriving (Eq, Ord, Prim, U.Unbox)

newtype instance U.MVector s PackedCharAndIdx = MV_PackedCharAndIdx (U.MVector s Word64)
newtype instance U.Vector    PackedCharAndIdx = V_PackedCharAndIdx  (U.Vector    Word64)
deriving instance GM.MVector U.MVector PackedCharAndIdx
deriving instance G.Vector   U.Vector  PackedCharAndIdx

instance Show PackedCharAndIdx where
  show (PackedCharAndIdx x) =
    show
      ( chr $ fromIntegral $ keepChar (PackedChar x) `unsafeShiftR` 32
      , showString "0x" . showHex (keepChar (PackedChar x) `unsafeShiftR` 32) $ []
      , keepIdx (PackedIdx x)
      )

instance Pretty PackedCharAndIdx where
  pretty = ppShow

{-# INLINE combineCharIdx #-}
combineCharIdx :: Word64 -> Word64 -> PackedCharAndIdx
-- Safe to omit anding with lower4Bytes because index is unlikely to reach a point where that
-- operation would have any effect
-- combineCharIdx c idx = (c `unsafeShiftL` 32) .|. (lower4Bytes .&. w64 idx)
combineCharIdx c idx = PackedCharAndIdx ((c `unsafeShiftL` 32) .|. idx)

newtype PackedChar = PackedChar { unPackedChar :: Word64 }
  deriving (U.Unbox)

instance Show PackedChar where
  showsPrec _ =
    (showString "0x" .) . showHex . (`unsafeShiftR` 32) . keepChar

newtype instance U.MVector s PackedChar = MV_PackedChar (U.MVector s PackedCharAndIdx)
newtype instance U.Vector    PackedChar = V_PackedChar  (U.Vector    PackedCharAndIdx)
deriving instance GM.MVector U.MVector PackedChar
deriving instance G.Vector   U.Vector  PackedChar

mkPackedChar :: Char -> PackedChar
mkPackedChar = PackedChar . (`unsafeShiftL` 32) . w64 . ord

keepChar :: PackedChar -> Word64
keepChar =
  (.&. upper4Bytes) . unPackedChar

instance Eq PackedChar where
  (==) = (==) `on` keepChar

instance Ord PackedChar where
  compare = compare `on` keepChar


newtype PackedIdx = PackedIdx { unPackedIdx :: Word64 }
  deriving (U.Unbox)

{-# INLINE upper4Bytes #-}
{-# INLINE lower4Bytes #-}
upper4Bytes, lower4Bytes :: Integral a => a
upper4Bytes = 0xFFFFFFFF00000000
lower4Bytes = 0x00000000FFFFFFFF

{-# INLINE keepIdx #-}
keepIdx :: PackedIdx -> StrIdx
keepIdx = StrIdx . fromIntegral . (.&. lower4Bytes) . unPackedIdx

{-# INLINE mkPackedIdx #-}
mkPackedIdx :: StrIdx -> PackedIdx
mkPackedIdx = PackedIdx . w64 . unStrIdx

{-# INLINE getStrIdx #-}
getStrIdx :: PackedIdx -> StrIdx
getStrIdx = keepIdx

newtype instance U.MVector s PackedIdx = MV_PackedIdx (U.MVector s PackedCharAndIdx)
newtype instance U.Vector    PackedIdx = V_PackedIdx  (U.Vector    PackedCharAndIdx)
deriving instance GM.MVector U.MVector PackedIdx
deriving instance G.Vector   U.Vector  PackedIdx

instance Show PackedIdx where
  show = show . keepIdx

instance Pretty PackedIdx where
  pretty = ppShow

instance Eq PackedIdx where
  (==) = (==) `on` keepIdx

instance Ord PackedIdx where
  compare = compare `on` keepIdx


{-# INLINE w64 #-}
w64 :: Integral a => a -> Word64
w64 = fromIntegral
