----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Growable
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns      #-}

module Data.Vector.Growable
  ( GrowableVector
  , new
  , push
  , unsafeFreeze
  , freeze
  , finalise
  ) where

import Control.Monad.Primitive
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM

data GrowableVector v = GrowableVector
  { gvSize  :: !Int -- Number of actual elements
  , gvStore :: !v
  }

{-# INLINE new #-}
new :: (PrimMonad m, GM.MVector v a) => Int -> m (GrowableVector (v (PrimState m) a))
new = fmap (GrowableVector 0) . GM.unsafeNew . max 1

{-# INLINE finalise #-}
finalise :: GM.MVector v a => GrowableVector (v s a) -> v s a
finalise GrowableVector{gvSize, gvStore} =
  GM.unsafeSlice 0 gvSize gvStore

{-# INLINE unsafeFreeze #-}
unsafeFreeze :: (PrimMonad m, G.Vector w a) => GrowableVector (G.Mutable w (PrimState m) a) -> m (w a)
unsafeFreeze = G.unsafeFreeze . finalise

{-# INLINE freeze #-}
freeze :: (PrimMonad m, G.Vector w a) => GrowableVector (G.Mutable w (PrimState m) a) -> m (w a)
freeze = G.freeze . finalise

{-# INLINE push #-}
push
  :: (PrimMonad m, GM.MVector v a)
  => a
  -> GrowableVector (v (PrimState m) a)
  -> m (GrowableVector (v (PrimState m) a))
push item GrowableVector{gvSize, gvStore} = do
  store <-
    if gvSize == GM.length gvStore
    then do
      GM.unsafeGrow gvStore gvSize
    else pure gvStore
  GM.unsafeWrite store gvSize item
  pure $ GrowableVector (gvSize + 1) store
