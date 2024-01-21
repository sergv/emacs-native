-- |
-- Module:     Data.StrCharIdx
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE TypeFamilies #-}

module Data.StrIdx
  ( StrCharIdx(..)
  ) where

import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Prettyprinter

newtype StrCharIdx a = StrCharIdx { unStrCharIdx :: a }
  deriving (Eq, Ord, Enum, Pretty)

deriving instance U.Unbox a => U.Unbox (StrCharIdx a)

instance Show a => Show (StrCharIdx a) where
  show = show . unStrCharIdx

newtype instance U.MVector s (StrCharIdx a) = MV_StrCharIdx (U.MVector s a)
newtype instance U.Vector    (StrCharIdx a) = V_StrCharIdx  (U.Vector    a)
deriving instance GM.MVector U.MVector a => GM.MVector U.MVector (StrCharIdx a)
deriving instance G.Vector   U.Vector  a => G.Vector   U.Vector  (StrCharIdx a)
