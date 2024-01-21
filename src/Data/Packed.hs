-- |
-- Module:     Data.Packed
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE TypeFamilies #-}

module Data.Packed
  ( PackedCharAndStrIdx(..)
  , combineCharIdx
  , PackedChar(..)
  , mkPackedChar
  , keepChar
  , PackedStrIdx(..)
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

newtype PackedCharAndStrIdx = PackedCharAndStrIdx { _unPackedCharAndIdx :: Word64 }
  deriving (Eq, Ord, Prim, U.Unbox)

newtype instance U.MVector s PackedCharAndStrIdx = MV_PackedCharAndIdx (U.MVector s Word64)
newtype instance U.Vector    PackedCharAndStrIdx = V_PackedCharAndIdx  (U.Vector    Word64)
deriving instance GM.MVector U.MVector PackedCharAndStrIdx
deriving instance G.Vector   U.Vector  PackedCharAndStrIdx

instance Show PackedCharAndStrIdx where
  show (PackedCharAndStrIdx x) =
    show
      ( chr $ fromIntegral $ keepChar (PackedChar x) `unsafeShiftR` 32
      , showString "0x" . showHex (keepChar (PackedChar x) `unsafeShiftR` 32) $ []
      , keepIdx (PackedStrIdx x)
      )

instance Pretty PackedCharAndStrIdx where
  pretty = ppShow

{-# INLINE combineCharIdx #-}
combineCharIdx :: Word64 -> Word64 -> PackedCharAndStrIdx
-- Safe to omit anding with lower4Bytes because index is unlikely to reach a point where that
-- operation would have any effect
-- combineCharIdx c idx = (c `unsafeShiftL` 32) .|. (lower4Bytes .&. w64 idx)
combineCharIdx c idx = PackedCharAndStrIdx ((c `unsafeShiftL` 32) .|. idx)

newtype PackedChar = PackedChar { unPackedChar :: Word64 }
  deriving (U.Unbox)

instance Show PackedChar where
  showsPrec _ =
    (showString "0x" .) . showHex . (`unsafeShiftR` 32) . keepChar

newtype instance U.MVector s PackedChar = MV_PackedChar (U.MVector s PackedCharAndStrIdx)
newtype instance U.Vector    PackedChar = V_PackedChar  (U.Vector    PackedCharAndStrIdx)
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


newtype PackedStrIdx = PackedStrIdx { unPackedIdx :: Word64 }
  deriving (U.Unbox)

{-# INLINE upper4Bytes #-}
{-# INLINE lower4Bytes #-}
upper4Bytes, lower4Bytes :: Integral a => a
upper4Bytes = 0xFFFFFFFF00000000
lower4Bytes = 0x00000000FFFFFFFF

{-# INLINE keepIdx #-}
keepIdx :: PackedStrIdx -> StrIdx
keepIdx = StrIdx . fromIntegral . (.&. lower4Bytes) . unPackedIdx

{-# INLINE mkPackedIdx #-}
mkPackedIdx :: StrIdx -> PackedStrIdx
mkPackedIdx = PackedStrIdx . w64 . unStrIdx

{-# INLINE getStrIdx #-}
getStrIdx :: PackedStrIdx -> StrIdx
getStrIdx = keepIdx

newtype instance U.MVector s PackedStrIdx = MV_PackedIdx (U.MVector s PackedCharAndStrIdx)
newtype instance U.Vector    PackedStrIdx = V_PackedIdx  (U.Vector    PackedCharAndStrIdx)
deriving instance GM.MVector U.MVector PackedStrIdx
deriving instance G.Vector   U.Vector  PackedStrIdx

instance Show PackedStrIdx where
  show = show . keepIdx

instance Pretty PackedStrIdx where
  pretty = ppShow

instance Eq PackedStrIdx where
  (==) = (==) `on` keepIdx

instance Ord PackedStrIdx where
  compare = compare `on` keepIdx


{-# INLINE w64 #-}
w64 :: Integral a => a -> Word64
w64 = fromIntegral
