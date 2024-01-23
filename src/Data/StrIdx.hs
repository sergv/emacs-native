-- |
-- Module:     Data.StrCharIdx
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE TypeFamilies #-}

module Data.StrIdx
  ( StrCharIdx(..)
  , charIdxAdvance
  , StrByteIdx(..)
  , byteIdxAdvance
  ) where

import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Prettyprinter

-- Character index.
newtype StrCharIdx a = StrCharIdx { unStrCharIdx :: a }
  deriving (Eq, Ord, Enum, Pretty, Functor, Foldable, Traversable)

deriving instance U.Unbox a => U.Unbox (StrCharIdx a)

instance Show a => Show (StrCharIdx a) where
  show = show . unStrCharIdx

{-# INLINE charIdxAdvance #-}
charIdxAdvance :: Num a => StrCharIdx a -> a -> StrCharIdx a
charIdxAdvance (StrCharIdx x) y = StrCharIdx (x + y)

newtype instance U.MVector s (StrCharIdx a) = MV_StrCharIdx (U.MVector s a)
newtype instance U.Vector    (StrCharIdx a) = V_StrCharIdx  (U.Vector    a)
deriving instance GM.MVector U.MVector a => GM.MVector U.MVector (StrCharIdx a)
deriving instance G.Vector   U.Vector  a => G.Vector   U.Vector  (StrCharIdx a)

-- Byte that points to start of utf8-encoded sequence.
newtype StrByteIdx a = StrByteIdx { unStrByteIdx :: a }
  deriving (Eq, Ord, Enum, Pretty, Functor, Foldable, Traversable)

{-# INLINE byteIdxAdvance #-}
byteIdxAdvance :: Num a => StrByteIdx a -> a -> StrByteIdx a
byteIdxAdvance (StrByteIdx x) y = StrByteIdx (x + y)

deriving instance U.Unbox a => U.Unbox (StrByteIdx a)

instance Show a => Show (StrByteIdx a) where
  show = show . unStrByteIdx

newtype instance U.MVector s (StrByteIdx a) = MV_StrByteIdx (U.MVector s a)
newtype instance U.Vector    (StrByteIdx a) = V_StrByteIdx  (U.Vector    a)
deriving instance GM.MVector U.MVector a => GM.MVector U.MVector (StrByteIdx a)
deriving instance G.Vector   U.Vector  a => G.Vector   U.Vector  (StrByteIdx a)
