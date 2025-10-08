-- |
-- Module:     Data.Ignores
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE OverloadedStrings #-}

module Data.Ignores
  ( Ignores
  , mkEmacsIgnores
  , mkIgnores
  , isIgnored
  , isIgnoredFile
  , dummyIgnores
  ) where

import Data.Coerce (coerce)
import Data.Filesystem.Find
import Data.Regex
import Data.Text (Text)
import Emacs.Module
import Emacs.Module.Assert
import System.Directory.OsPath.Types
import System.OsPath.Types (OsPath)

data Ignores = Ignores
  { ignoresBasenameRE :: !RegexSet
  , ignoresAbsRE      :: !RegexSet
  }

mkEmacsIgnores
  :: ( WithCallStack
     , MonadEmacs m v
     , MonadThrow (m s)
     )
  => v s
  -> v s
  -> v s
  -> v s
  -> m s (Ignores, Ignores)
mkEmacsIgnores ignoredFileGlobs ignoredDirGlobs ignoredDirPrefixes ignoredAbsDirs = do
  ignoredFileGlobs'   <- extractListWith extractText ignoredFileGlobs
  ignoredDirGlobs'    <- extractListWith extractText ignoredDirGlobs
  ignoredDirPrefixes' <- extractListWith extractText ignoredDirPrefixes
  ignoredAbsDirs'     <- extractListWith extractText ignoredAbsDirs
  fileIgnores         <- mkIgnores ignoredFileGlobs' []
  dirIgnores          <- mkIgnores ignoredAbsDirs' (coerce (ignoredDirGlobs' ++ map (<> "*") ignoredDirPrefixes'))
  pure (fileIgnores, dirIgnores)

mkIgnores
  :: ( WithCallStack
     , Functor f
     , Foldable f
     , MonadThrow m
     )
  => f Text
  -> f (Basename Text)
  -> m Ignores
mkIgnores ignoredAbsGlobs ignoredBasenameGlobs = do
  ignoresAbsRE      <- fileGlobsToRegex ignoredAbsGlobs
  ignoresBasenameRE <- fileGlobsToRegex ignoredBasenameGlobs
  pure Ignores{ignoresAbsRE, ignoresBasenameRE}

isIgnored :: Ignores -> OsPath -> Basename OsPath -> Bool
isIgnored Ignores{ignoresAbsRE, ignoresBasenameRE} absPath (Basename basePath) =
  reSetMatchesOsPath ignoresBasenameRE basePath ||
    reSetMatchesOsPath ignoresAbsRE absPath

isIgnoredFile :: Ignores -> AbsFile -> Bool
isIgnoredFile Ignores{ignoresAbsRE} (AbsFile absPath) =
  reSetMatchesOsPath ignoresAbsRE absPath

dummyIgnores :: Ignores
dummyIgnores = Ignores
  { ignoresAbsRE      = dummyRe
  , ignoresBasenameRE = dummyRe
  }
  where
    dummyRe :: RegexSet
    dummyRe = case compileReSet ["^()$"] of
      Nothing -> error "Invalid dummy regex"
      Just x  -> x
