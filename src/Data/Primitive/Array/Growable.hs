----------------------------------------------------------------------------
-- |
-- Module      :  Data.Primitive.Array.Growable
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTSyntax          #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# LANGUAGE UnliftedDatatypes   #-}
{-# LANGUAGE UnliftedNewtypes    #-}

module Data.Primitive.Array.Growable
  ( GrowableArray
  , new
  , push
  , size
  , unsafeToVector
  , clear
  ) where

import Control.Monad.Primitive
import Data.Primitive.Array
import Data.Vector qualified as V

data GrowableArray s a where -- :: TYPE (BoxedRep Unlifted) where
  GrowableArray ::
    { gaSize  :: {-# UNPACK #-} !Int -- Number of actual elements
    , gaStore :: {-# UNPACK #-} !(MutableArray s a)
    } -> GrowableArray s a

{-# INLINE new #-}
new :: PrimMonad m => Int -> m (GrowableArray (PrimState m) a)
new n = fmap (GrowableArray 0) $ newArray (max 1 n) (error "uninitialised")

unsafeToVector :: PrimMonad m => GrowableArray (PrimState m) a -> m (V.Vector a)
unsafeToVector GrowableArray{gaSize, gaStore} = do
  store <- unsafeFreezeArray gaStore
  pure $! V.unsafeFromArraySlice store 0 gaSize

{-# INLINE size #-}
size :: GrowableArray s a -> Int
size = gaSize

{-# INLINE push #-}
push
  :: PrimMonad m
  => a
  -> GrowableArray (PrimState m) a
  -> m (GrowableArray (PrimState m) a)
push item GrowableArray{gaSize, gaStore} = do
  let len = sizeofMutableArray gaStore
  store <-
    if gaSize == len
    then do
      store' <- newArray (gaSize * 2) (error "uninitialised")
      -- resizeMutableArray gaStore (gaSize * 2)
      copyMutableArray store' 0 gaStore 0 gaSize
      pure store'
    else
      pure gaStore
  writeArray store gaSize item
  pure $ GrowableArray (gaSize + 1) store

clear
  :: GrowableArray s a
  -> GrowableArray s a
clear x = x { gaSize = 0 }
