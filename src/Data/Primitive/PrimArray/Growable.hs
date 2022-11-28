----------------------------------------------------------------------------
-- |
-- Module      :  Data.Primitive.PrimArray.Growable
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

module Data.Primitive.PrimArray.Growable
  ( GrowablePrimArray
  , new
  , push
  , unsafeFreeze
  , size
  -- , freeze
  , finalise
  , clear
  , GrowablePrimArrayU
  , toUnboxed
  , fromUnboxed
  , pushU
  ) where

-- import qualified Debug.Trace

import Control.Monad
import Control.Monad.Primitive
import Data.Primitive.PrimArray
import Data.Primitive.Types
import GHC.Exts

newtype GrowablePrimArrayU s a = GrowablePrimArrayU
  { _unGrowablePrimArrayU :: (# Int#, MutableByteArray# s #) }

toUnboxed :: GrowablePrimArray s a -> GrowablePrimArrayU s a
toUnboxed (GrowablePrimArray (I# n) (MutablePrimArray arr)) =
  GrowablePrimArrayU (# n, arr #)

fromUnboxed :: GrowablePrimArrayU s a -> GrowablePrimArray s a
fromUnboxed (GrowablePrimArrayU (# n, arr #)) =
  GrowablePrimArray (I# n) (MutablePrimArray arr)

{-# INLINE pushU #-}
pushU
  :: forall a s. Prim a
  => a
  -> GrowablePrimArrayU s a
  -> State# s
  -> (# State# s, GrowablePrimArrayU s a #)
pushU item (GrowablePrimArrayU (# n, arr1 #)) s1 =
  case getSizeofMutableByteArray# arr1 s1 of
    (# s2, sz #) ->
      let !k              = n *# (sizeOf# (undefined :: a))
          !(# s3, arr2 #) =
            if isTrue# (sz ==# k)
            then
              resizeMutableByteArray# arr1 (sz *# 2#) s2
            else
              (# s2, arr1 #)
      in
        case writeByteArray# arr2 n item s3 of
          s4 ->
            (# s4, GrowablePrimArrayU (# n +# 1#, arr2 #) #)

      -- -- (# s'#, I# (quotInt# sz# (sizeOf# (undefined :: a))) #)
      --
  -- n   <- getSizeofMutablePrimArray store
  -- store <-
  --   if gpaSize == n
  --   then
  --     resizeMutablePrimArray store (gpaSize * 2)
  --   else
  --     pure store

  -- writePrimArray store gpaSize item
  -- pure $ GrowablePrimArray (gpaSize + 1) store

data GrowablePrimArray s a where -- :: TYPE (BoxedRep Unlifted) where
  GrowablePrimArray ::
    { gpaSize  :: {-# UNPACK #-} !Int -- Number of actual elements
    , gpaStore :: {-# UNPACK #-} !(MutablePrimArray s a)
    } -> GrowablePrimArray s a

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
