-- |
-- Module:     Data.Packed
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE TypeFamilies #-}

module Data.Packed
  ( PackedCharAndStrCharIdx(..)
  , combineCharIdx
  , PackedChar(..)
  , mkPackedChar
  , keepChar
  , PackedStrCharIdx(..)
  , keepIdx
  , mkPackedIdx
  , getStrCharIdx

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

newtype PackedCharAndStrCharIdx = PackedCharAndStrCharIdx { _unPackedCharAndIdx :: Word64 }
  deriving (Eq, Ord, Prim, U.Unbox)

newtype instance U.MVector s PackedCharAndStrCharIdx = MV_PackedCharAndIdx (U.MVector s Word64)
newtype instance U.Vector    PackedCharAndStrCharIdx = V_PackedCharAndIdx  (U.Vector    Word64)
deriving instance GM.MVector U.MVector PackedCharAndStrCharIdx
deriving instance G.Vector   U.Vector  PackedCharAndStrCharIdx

instance Show PackedCharAndStrCharIdx where
  show (PackedCharAndStrCharIdx x) =
    show
      ( chr $ fromIntegral $ keepChar (PackedChar x) `unsafeShiftR` 32
      , showString "0x" . showHex (keepChar (PackedChar x) `unsafeShiftR` 32) $ []
      , keepIdx (PackedStrCharIdx x)
      )

instance Pretty PackedCharAndStrCharIdx where
  pretty = ppShow

{-# INLINE combineCharIdx #-}
combineCharIdx :: Word64 -> Word64 -> PackedCharAndStrCharIdx
-- Safe to omit anding with lower4Bytes because index is unlikely to reach a point where that
-- operation would have any effect
-- combineCharIdx c idx = (c `unsafeShiftL` 32) .|. (lower4Bytes .&. w64 idx)
combineCharIdx c idx = PackedCharAndStrCharIdx ((c `unsafeShiftL` 32) .|. idx)

newtype PackedChar = PackedChar { unPackedChar :: Word64 }
  deriving (U.Unbox)

instance Show PackedChar where
  showsPrec _ =
    (showString "0x" .) . showHex . (`unsafeShiftR` 32) . keepChar

newtype instance U.MVector s PackedChar = MV_PackedChar (U.MVector s PackedCharAndStrCharIdx)
newtype instance U.Vector    PackedChar = V_PackedChar  (U.Vector    PackedCharAndStrCharIdx)
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


newtype PackedStrCharIdx = PackedStrCharIdx { unPackedIdx :: Word64 }
  deriving (U.Unbox)

{-# INLINE upper4Bytes #-}
{-# INLINE lower4Bytes #-}
upper4Bytes, lower4Bytes :: Integral a => a
upper4Bytes = 0xFFFFFFFF00000000
lower4Bytes = 0x00000000FFFFFFFF

{-# INLINE keepIdx #-}
keepIdx :: PackedStrCharIdx -> StrCharIdx
keepIdx = StrCharIdx . fromIntegral . (.&. lower4Bytes) . unPackedIdx

{-# INLINE mkPackedIdx #-}
mkPackedIdx :: StrCharIdx -> PackedStrCharIdx
mkPackedIdx = PackedStrCharIdx . w64 . unStrCharIdx

{-# INLINE getStrCharIdx #-}
getStrCharIdx :: PackedStrCharIdx -> StrCharIdx
getStrCharIdx = keepIdx

newtype instance U.MVector s PackedStrCharIdx = MV_PackedIdx (U.MVector s PackedCharAndStrCharIdx)
newtype instance U.Vector    PackedStrCharIdx = V_PackedIdx  (U.Vector    PackedCharAndStrCharIdx)
deriving instance GM.MVector U.MVector PackedStrCharIdx
deriving instance G.Vector   U.Vector  PackedStrCharIdx

instance Show PackedStrCharIdx where
  show = show . keepIdx

instance Pretty PackedStrCharIdx where
  pretty = ppShow

instance Eq PackedStrCharIdx where
  (==) = (==) `on` keepIdx

instance Ord PackedStrCharIdx where
  compare = compare `on` keepIdx


{-# INLINE w64 #-}
w64 :: Integral a => a -> Word64
w64 = fromIntegral
