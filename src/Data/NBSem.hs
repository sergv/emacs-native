----------------------------------------------------------------------------
-- |
-- Module      :  Data.NBSem
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :   5 March 2018
----------------------------------------------------------------------------

module Data.NBSem
  ( NBSem
  , newNBSem
  , tryAcquireNBSem
  , releaseNBSem
  ) where

import Control.Concurrent.STM

newtype NBSem = NBSem (TVar Int)

{-# INLINE newNBSem #-}
newNBSem :: Int -> IO NBSem
newNBSem i = NBSem <$> newTVarIO i

{-# INLINE tryAcquireNBSem #-}
tryAcquireNBSem :: NBSem -> IO Bool
tryAcquireNBSem (NBSem m) = atomically $ do
  i <- readTVar m
  if i == 0
  then pure False
  else do
    writeTVar m $! i - 1
    pure True

{-# INLINE releaseNBSem #-}
releaseNBSem :: NBSem -> IO ()
releaseNBSem (NBSem m) =
  atomically $ modifyTVar m (+ 1)
