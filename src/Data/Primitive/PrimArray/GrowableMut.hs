----------------------------------------------------------------------------
-- |
-- Module      :  Data.Primitive.PrimArray.GrowableMut
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Primitive.PrimArray.GrowableMut
  ( GrowablePrimArrayMut
  , new
  , push
  , size
  -- , unsafeFreeze
  , finalise
  , clear
  , with
  ) where

import Control.Monad
import Control.Monad.Primitive
import Data.Primitive.PrimArray
import Data.Primitive.Types
import Data.STRef

import Data.Primitive.PrimArray.Growable qualified as PG

newtype GrowablePrimArrayMut s a = GrowablePrimArrayMut
  { unGrowablePrimArrayMut :: STRef s (PG.GrowablePrimArray s a) }

{-# INLINE new #-}
new :: (Prim a, PrimMonad m) => Int -> m (GrowablePrimArrayMut (PrimState m) a)
new n = do
  ref <- stToPrim . newSTRef =<< PG.new n
  pure $ GrowablePrimArrayMut ref

size :: forall m a. PrimMonad m => GrowablePrimArrayMut (PrimState m) a -> m Int
size = fmap PG.size . (stToPrim . readSTRef) . unGrowablePrimArrayMut

{-# INLINE finalise #-}
finalise
  :: (Prim a, PrimMonad m)
  => GrowablePrimArrayMut (PrimState m) a
  -> m (MutablePrimArray (PrimState m) a)
finalise =
  (PG.finalise <=< stToPrim . readSTRef) . unGrowablePrimArrayMut

-- {-# INLINE unsafeFreeze #-}
-- unsafeFreeze :: (Prim a, PrimMonad m) => GrowablePrimArrayMut (PrimState m) a -> m (PrimArray a)
-- unsafeFreeze =
--   (PG.unsafeFreeze <=< stToPrim . readSTRef) . unGrowablePrimArrayMut

-- {-# INLINE freeze #-}
-- freeze :: (PrimMonad m, Prim a) => GrowablePrimArray (PrimState m) a -> m (w a)
-- freeze = G.freeze <=< finalise

{-# INLINE push #-}
push
  :: (Prim a, PrimMonad m)
  => a
  -> GrowablePrimArrayMut (PrimState m) a
  -> m ()
push a (GrowablePrimArrayMut ref) = do
  stToPrim . writeSTRef ref =<< PG.push a =<< stToPrim (readSTRef ref)

{-# INLINE clear #-}
clear :: (Prim a, PrimMonad m) => GrowablePrimArrayMut (PrimState m) a -> m ()
clear (GrowablePrimArrayMut ref) =
  stToPrim . writeSTRef ref . PG.clear =<< stToPrim (readSTRef ref)

{-# INLINE with #-}
with
  :: (Prim a, PrimMonad m)
  => GrowablePrimArrayMut (PrimState m) a
  -> (PG.GrowablePrimArray (PrimState m) a -> m (b, PG.GrowablePrimArray (PrimState m) a))
  -> m b
with (GrowablePrimArrayMut ref) f =
  (\(b, arr) -> b <$ stToPrim (writeSTRef ref arr)) =<< f =<< stToPrim (readSTRef ref)
