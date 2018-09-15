----------------------------------------------------------------------------
-- |
-- Module      :  Data.Emacs.Path
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

module Data.Emacs.Path (pathForEmacs) where

import qualified Data.ByteString.Char8 as C8
import Path

pathForEmacs :: Path a b -> C8.ByteString
pathForEmacs = normaliseSeparators . C8.pack . toFilePath
  where
    normaliseSeparators =
#ifdef mingw32_HOST_OS
      C8.map (\case { '\\' -> '/'; c -> c })
#else
      id
#endif
