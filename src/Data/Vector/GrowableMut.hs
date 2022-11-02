----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Growable
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns      #-}

module Data.Vector.GrowableMut
  ( GrowableVectorMut
  , new
  , push
  , unsafeFreeze
  , freeze
  , finalise
  ) where

import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM

import Data.Vector.Growable qualified as VG

newtype GrowableVectorMut s v = GrowableVectorMut
  { unGrowableVectorMut :: STRef s (VG.GrowableVector v) }

{-# INLINE new #-}
new :: GM.MVector v a => Int -> ST s (GrowableVectorMut s (v s a))
new n = do
  ref <- newSTRef =<< VG.new n
  pure $ GrowableVectorMut ref

{-# INLINE finalise #-}
finalise :: GM.MVector v a => GrowableVectorMut s (v s a) -> ST s (v s a)
finalise =
  fmap VG.finalise . readSTRef . unGrowableVectorMut

{-# INLINE unsafeFreeze #-}
unsafeFreeze :: G.Vector w a => GrowableVectorMut s (G.Mutable w s a) -> ST s (w a)
unsafeFreeze =
  VG.unsafeFreeze <=< readSTRef . unGrowableVectorMut

{-# INLINE freeze #-}
freeze :: G.Vector w a => GrowableVectorMut s (G.Mutable w s a) -> ST s (w a)
freeze =
  VG.freeze <=< readSTRef . unGrowableVectorMut

{-# INLINE push #-}
push
  :: GM.MVector v a
  => a
  -> GrowableVectorMut s (v s a)
  -> ST s ()
push item (GrowableVectorMut ref) =
  writeSTRef ref =<< VG.push item =<< readSTRef ref
