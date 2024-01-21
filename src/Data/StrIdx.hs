-- |
-- Module:     Data.StrCharIdx
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE TypeFamilies #-}

module Data.StrIdx
  ( StrCharIdx(..)
  ) where

import Data.Int
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Base qualified as U
import Prettyprinter

newtype StrCharIdx = StrCharIdx { unStrCharIdx :: Int32 }
  deriving (Eq, Ord, Enum, Pretty, U.Unbox)

instance Show StrCharIdx where
  show = show . unStrCharIdx

newtype instance U.MVector s StrCharIdx = MV_StrCharIdx (U.MVector s Int32)
newtype instance U.Vector    StrCharIdx = V_StrCharIdx  (U.Vector    Int32)
deriving instance GM.MVector U.MVector StrCharIdx
deriving instance G.Vector   U.Vector  StrCharIdx
