-- |
-- Module:     Data.StrIdx
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE TypeFamilies #-}

module Data.StrIdx
  ( StrIdx(..)
  ) where

import Data.Int
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Base qualified as U
import Prettyprinter

newtype StrIdx = StrIdx { unStrIdx :: Int32 }
  deriving (Eq, Ord, Enum, Pretty, U.Unbox)

instance Show StrIdx where
  show = show . unStrIdx

newtype instance U.MVector s StrIdx = MV_StrIdx (U.MVector s Int32)
newtype instance U.Vector    StrIdx = V_StrIdx  (U.Vector    Int32)
deriving instance GM.MVector U.MVector StrIdx
deriving instance G.Vector   U.Vector  StrIdx
