----------------------------------------------------------------------------
-- |
-- Module      :  Data.ByteString.Char8.Ext
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

module Data.ByteString.Char8.Ext
  ( take
  , drop
  , splitAt
  , foldCharsM
  ) where

import Control.Applicative
import Control.Monad
import Control.Exception (evaluate)
import Data.ByteString.Char8 qualified as C8
import Foreign.C.Types (CChar)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek)

#if !defined(RUNTIME_CHECKS)
import Data.ByteString.Unsafe qualified as Unsafe
#endif

import Prelude (Int, IO, otherwise, Eq(..), Num(..))

take :: Int -> C8.ByteString -> C8.ByteString
take =
#if defined(RUNTIME_CHECKS)
  C8.take
#else
  Unsafe.unsafeTake
#endif

drop :: Int -> C8.ByteString -> C8.ByteString
drop =
#if defined(RUNTIME_CHECKS)
  C8.drop
#else
  Unsafe.unsafeDrop
#endif

splitAt :: Int -> C8.ByteString -> (C8.ByteString, C8.ByteString)
splitAt =
#if defined(RUNTIME_CHECKS)
  C8.splitAt
#else
  \n str -> (Unsafe.unsafeTake n str, Unsafe.unsafeDrop n str)
#endif

{-# INLINE foldCharsM #-}
foldCharsM :: forall a. Ptr CChar -> Int -> a -> (a -> CChar -> IO a) -> IO a
foldCharsM !p !len initAcc f =
  go p initAcc
  where
    !end = p `plusPtr` (len - 1)
    go :: Ptr CChar -> a -> IO a
    go !p' !acc
      | p' == end = pure acc
      | otherwise = do
        !c    <- peek p'
        !acc' <- evaluate =<< f acc c
        go (plusPtr p' 1) acc'
