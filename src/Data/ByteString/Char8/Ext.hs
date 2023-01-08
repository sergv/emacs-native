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
  ) where

import Data.ByteString.Char8 qualified as C8

#if !defined(RUNTIME_CHECKS)
import Data.ByteString.Unsafe qualified as Unsafe
#endif

import Prelude (Int)

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
