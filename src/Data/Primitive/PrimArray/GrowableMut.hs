----------------------------------------------------------------------------
-- |
-- Module      :  Data.Primitive.PrimArray.GrowableMut
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE ImportQualifiedPost #-}

module Data.Primitive.PrimArray.GrowableMut
  ( GrowablePrimArrayMut
  , new
  , push
  , unsafeFreeze
  , finalise
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Primitive.PrimArray
import Data.Primitive.Types
import Data.STRef

import Data.Primitive.PrimArray.Growable qualified as PG

newtype GrowablePrimArrayMut s a = GrowablePrimArrayMut
  { unGrowablePrimArrayMut :: STRef s (PG.GrowablePrimArray s a) }

{-# INLINE new #-}
new :: Prim a => Int -> ST s (GrowablePrimArrayMut s a)
new n = do
  ref <- newSTRef =<< PG.new n
  pure $ GrowablePrimArrayMut ref

{-# INLINE finalise #-}
finalise
  :: Prim a
  => GrowablePrimArrayMut s a
  -> ST s (MutablePrimArray s a)
finalise =
  (PG.finalise <=< readSTRef) . unGrowablePrimArrayMut

{-# INLINE unsafeFreeze #-}
unsafeFreeze :: Prim a => GrowablePrimArrayMut s a -> ST s (PrimArray a)
unsafeFreeze =
  (PG.unsafeFreeze <=< readSTRef) . unGrowablePrimArrayMut

-- {-# INLINE freeze #-}
-- freeze :: (PrimMonad m, Prim a) => GrowablePrimArray (PrimState m) a -> m (w a)
-- freeze = G.freeze <=< finalise

{-# INLINE push #-}
push
  :: Prim a
  => a
  -> GrowablePrimArrayMut s a
  -> ST s ()
push a (GrowablePrimArrayMut ref) = do
  writeSTRef ref =<< PG.push a =<< readSTRef ref
