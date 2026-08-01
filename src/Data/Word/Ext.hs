-- |
-- Module:     Data.Word.Ext
-- Copyright:  (c) Sergey Vinokurov 2026
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE CPP #-}

module Data.Word.Ext
  ( getWord32LE
  ) where

import Data.Word
import Foreign.Ptr
import GHC.Storable (readWord32OffPtr)

#include "MachDeps.h"

getWord32LE :: Ptr Word32 -> IO Word32
getWord32LE ptr =
#if defined(WORDS_BIGENDIAN)
  byteSwap32 <$> readWord32OffPtr ptr 0
#endif
#if !defined(WORDS_BIGENDIAN)
  readWord32OffPtr ptr 0
#endif
