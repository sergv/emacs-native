----------------------------------------------------------------------------
-- |
-- Module      :  Data.Primitive.PrimArray.Growable
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}

module Data.Primitive.PrimArray.Growable
  ( GrowablePrimArray
  , new
  , push
  , unsafeFreeze
  , size
  -- , freeze
  , finalise
  , clear
  ) where

-- import qualified Debug.Trace

import Control.Monad
import Control.Monad.Primitive
import Data.Primitive.PrimArray
import Data.Primitive.Types

data GrowablePrimArray s a = GrowablePrimArray
  { gpaSize  :: {-# UNPACK #-} !Int -- Number of actual elements
  , gpaStore :: {-# UNPACK #-} !(MutablePrimArray s a)
  }

{-# INLINE new #-}
new :: (PrimMonad m, Prim a) => Int -> m (GrowablePrimArray (PrimState m) a)
new = fmap (GrowablePrimArray 0) . newPrimArray . max 1

{-# INLINE finalise #-}
finalise
  :: (PrimMonad m, Prim a)
  => GrowablePrimArray (PrimState m) a
  -> m (MutablePrimArray (PrimState m) a)
finalise GrowablePrimArray{gpaSize, gpaStore} =
  gpaStore <$ shrinkMutablePrimArray gpaStore gpaSize

{-# INLINE unsafeFreeze #-}
unsafeFreeze :: (PrimMonad m, Prim a) => GrowablePrimArray (PrimState m) a -> m (PrimArray a)
unsafeFreeze = unsafeFreezePrimArray <=< finalise

{-# INLINE size #-}
size :: GrowablePrimArray s a -> Int
size = gpaSize

-- {-# INLINE freeze #-}
-- freeze :: (PrimMonad m, Prim a) => GrowablePrimArray (PrimState m) a -> m (w a)
-- freeze = G.freeze <=< finalise

{-# INLINE push #-}
push
  :: (PrimMonad m, Prim a)
  => a
  -> GrowablePrimArray (PrimState m) a
  -> m (GrowablePrimArray (PrimState m) a)
push item GrowablePrimArray{gpaSize, gpaStore} = do
  len   <- getSizeofMutablePrimArray gpaStore
  store <-
    if gpaSize == len
    then
      resizeMutablePrimArray gpaStore (gpaSize * 2)
    else
      pure gpaStore
  writePrimArray store gpaSize item
  pure $ GrowablePrimArray (gpaSize + 1) store

clear
  :: GrowablePrimArray s a
  -> GrowablePrimArray s a
clear gpa = do -- @GrowablePrimArray{gpaStore} = do
  -- shrinkMutablePrimArray gpaStore 1
  gpa { gpaSize = 0 }