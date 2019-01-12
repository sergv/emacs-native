----------------------------------------------------------------------------
-- |
-- Module      :  Data.NBSem
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :   5 March 2018
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.NBSem
  ( NBSem
  , newNBSem
  , tryAcquireNBSem
  , releaseNBSem
  ) where

import Data.IORef

newtype NBSem = NBSem (IORef Int)

{-# INLINE newNBSem #-}
newNBSem :: Int -> IO NBSem
newNBSem i = NBSem <$> newIORef i

{-# INLINE tryAcquireNBSem #-}
tryAcquireNBSem :: NBSem -> IO Bool
tryAcquireNBSem (NBSem m) =
  atomicModifyIORef' m $ \i ->
    if i == 0
    then (i, False)
    else let !z = i - 1 in (z, True)

{-# INLINE releaseNBSem #-}
releaseNBSem :: NBSem -> IO ()
releaseNBSem (NBSem m) =
  atomicModifyIORef m $ \i ->
    let !z = i + 1 in (z, ())
