-- |
-- Module:     Data.MinMaxIdx
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.MinMaxIdx
  ( MinMaxIdx
  , mkMinMaxIdx
  , getMinMax
  ) where

import Prelude hiding (min, max, init)
import Prelude qualified

import Control.LensBlaze
import Data.Int
import Data.Word

newtype MinMaxIdx = MinMaxIdx { unMinMaxIdx :: Word64 }
  deriving (Eq, Ord, Show)

{-# INLINE minMaxL #-}
minMaxL :: Lens' MinMaxIdx Word64
minMaxL = lens unMinMaxIdx (\x _ -> MinMaxIdx x)

{-# INLINE minL #-}
minL :: Lens' MinMaxIdx Int32
minL = minMaxL . int32L 0

{-# INLINE maxL #-}
maxL :: Lens' MinMaxIdx Int32
maxL = minMaxL . int32L 32

instance Semigroup MinMaxIdx where
  x <> y = mkMinMaxIdx (Prelude.min minX minY) (Prelude.max maxX maxY)
    where
      (minX, maxX) = getMinMax x
      (minY, maxY) = getMinMax y

mkMinMaxIdx :: Int32 -> Int32 -> MinMaxIdx
mkMinMaxIdx min max =
  set minL min $ set maxL max init
  where
    init :: MinMaxIdx
    init = MinMaxIdx 0

{-# INLINE getMinMax #-}
getMinMax :: MinMaxIdx -> (Int32, Int32)
getMinMax x = (view minL x, view maxL x)
