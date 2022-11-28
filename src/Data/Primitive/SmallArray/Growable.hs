----------------------------------------------------------------------------
-- |
-- Module      :  Data.Smallitive.SmallArray.Growable
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

module Data.Primitive.SmallArray.Growable
  ( SmallArray
  , GrowableSmallArray
  , new
  , push
  , unsafeFreeze
  , size
  -- , freeze
  , finalise
  , clear
  ) where

import Control.Monad
import Control.Monad.Primitive
import Data.Primitive.SmallArray

data GrowableSmallArray s a where -- :: TYPE (BoxedRep Unlifted) where
  GrowableSmallArray ::
    { gsaSize  :: {-# UNPACK #-} !Int -- Number of actual elements
    , gsaStore :: {-# UNPACK #-} !(SmallMutableArray s a)
    } -> GrowableSmallArray s a

{-# INLINE new #-}
new :: PrimMonad m => Int -> m (GrowableSmallArray (PrimState m) a)
new n = fmap (GrowableSmallArray 0) $ newSmallArray (max 1 n)  (error "uninitialized")

{-# INLINE finalise #-}
finalise
  :: PrimMonad m
  => GrowableSmallArray (PrimState m) a
  -> m (SmallMutableArray (PrimState m) a)
finalise GrowableSmallArray{gsaSize, gsaStore} =
  gsaStore <$ shrinkSmallMutableArray gsaStore gsaSize

{-# INLINE unsafeFreeze #-}
unsafeFreeze :: PrimMonad m => GrowableSmallArray (PrimState m) a -> m (SmallArray a)
unsafeFreeze = unsafeFreezeSmallArray <=< finalise

{-# INLINE size #-}
size :: GrowableSmallArray s a -> Int
size = gsaSize

-- {-# INLINE freeze #-}
-- freeze :: PrimMonad m => GrowableSmallArray (PrimState m) a -> m (w a)
-- freeze = G.freeze <=< finalise

{-# INLINE push #-}
push
  :: PrimMonad m
  => a
  -> GrowableSmallArray (PrimState m) a
  -> m (GrowableSmallArray (PrimState m) a)
push item GrowableSmallArray{gsaSize, gsaStore} = do
  let !len = sizeofSmallMutableArray gsaStore
  store <-
    if gsaSize == len
    then
      resizeSmallMutableArray gsaStore (gsaSize * 2) (error "uninitialized")
    else
      pure gsaStore
  writeSmallArray store gsaSize item
  pure $ GrowableSmallArray (gsaSize + 1) store

clear
  :: GrowableSmallArray s a
  -> GrowableSmallArray s a
clear x = x { gsaSize = 0 }
