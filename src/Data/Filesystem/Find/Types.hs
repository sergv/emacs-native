-- |
-- Module:     Data.Filesystem.Types
-- Copyright:  (c) Sergey Vinokurov 2026
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE DerivingVia #-}

module Data.Filesystem.Find.Types
  ( FollowSymlinks(..)
  , AbsDir(..)
  , RelDir(..)
  , AbsFile(..)
  , RelFile(..)
  ) where

import Prettyprinter.Show
import System.Directory.OsPath.Streaming as Streaming
import System.OsPath

data FollowSymlinks a
  = -- | Recurse into symlinked directories
    FollowSymlinks
  | -- | Do not recurse into symlinked directories, but possibly report them.
    -- Function receives absolute directory name and its basename part.
    ReportSymlinks (OsPath -> Basename OsPath -> IO (Maybe a))

newtype AbsDir  = AbsDir  { unAbsDir  :: OsPath }
  deriving (Eq, Show)
  deriving Pretty via PPShow AbsDir

newtype RelDir  = RelDir  { unRelDir  :: OsPath }
  deriving (Eq, Show)
  deriving Pretty via PPShow RelDir

newtype AbsFile = AbsFile { unAbsFile :: OsPath }
  deriving (Eq, Show)
  deriving Pretty via PPShow AbsFile

newtype RelFile = RelFile { unRelFile :: OsPath }
  deriving (Eq, Show)
  deriving Pretty via PPShow RelFile


