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
import Data.Coerce
import Data.Int
import Data.Word
import Prettyprinter
import Prettyprinter.Show

newtype MinMaxIdx f = MinMaxIdx { unMinMaxIdx :: Word64 }
  deriving (Eq, Ord)

instance (Coercible (f Int32) Int32, Show (f Int32)) => Show (MinMaxIdx f) where
  show = show . getMinMax

instance (Coercible (f Int32) Int32, Show (f Int32)) => Pretty (MinMaxIdx f) where
  pretty = ppShow

{-# INLINE minMaxL #-}
minMaxL :: Lens' (MinMaxIdx f) Word64
minMaxL = lens unMinMaxIdx (\x _ -> MinMaxIdx x)

{-# INLINE minL #-}
minL :: Coercible (f Int32) Int32 => Lens' (MinMaxIdx f) (f Int32)
minL = minMaxL . int32L 0 . coerceL

{-# INLINE maxL #-}
maxL :: Coercible (f Int32) Int32 => Lens' (MinMaxIdx f) (f Int32)
maxL = minMaxL . int32L 32 . coerceL

instance (Coercible (f Int32) Int32, Ord (f Int32)) => Semigroup (MinMaxIdx f) where
  x <> y = mkMinMaxIdx (Prelude.min minX minY) (Prelude.max maxX maxY)
    where
      (minX, maxX) = getMinMax x
      (minY, maxY) = getMinMax y

mkMinMaxIdx :: forall f. Coercible (f Int32) Int32 => f Int32 -> f Int32 -> MinMaxIdx f
mkMinMaxIdx min max =
  set minL min $ set maxL max init
  where
    init :: MinMaxIdx f
    init = MinMaxIdx 0

{-# INLINE getMinMax #-}
getMinMax :: Coercible (f Int32) Int32 => MinMaxIdx f -> (f Int32, f Int32)
getMinMax x = (view minL x, view maxL x)
