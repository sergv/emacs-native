-- |
-- Module:     Data.Bits.Ext
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.Bits.Ext
  ( upper4Bytes
  , lower4Bytes
  , w64
  ) where

import Data.Word

{-# INLINE upper4Bytes #-}
{-# INLINE lower4Bytes #-}
upper4Bytes, lower4Bytes :: Integral a => a
upper4Bytes = 0xFFFFFFFF00000000
lower4Bytes = 0x00000000FFFFFFFF

{-# INLINE w64 #-}
w64 :: Integral a => a -> Word64
w64 = fromIntegral

