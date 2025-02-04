-- |
-- Module:     Data.Ignores
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE OverloadedStrings #-}

module Data.Ignores
  ( Ignores
  , mkEmacsIgnores
  , shouldVisit
  , isIgnoredFile
  , dummyIgnores
  ) where

import Data.Filesystem.Find
import Data.Regex
import Data.Text (Text)
import Emacs.Module
import Emacs.Module.Assert

data Ignores = Ignores
  { ignoresFilesRE   :: !Regex
  , ignoresDirsRE    :: !Regex
  , ignoresAbsDirsRE :: !Regex
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
  -> m s Ignores
mkEmacsIgnores ignoredFileGlobs ignoredDirGlobs ignoredDirPrefixes ignoredAbsDirs = do
  ignoredFileGlobs'   <- extractListWith extractText ignoredFileGlobs
  ignoredDirGlobs'    <- extractListWith extractText ignoredDirGlobs
  ignoredDirPrefixes' <- extractListWith extractText ignoredDirPrefixes
  ignoredAbsDirs'     <- extractListWith extractText ignoredAbsDirs
  mkIgnores ignoredFileGlobs' (ignoredDirGlobs' ++ map (<> "*") ignoredDirPrefixes') ignoredAbsDirs'

mkIgnores
  :: ( WithCallStack
     , Functor f
     , Foldable f
     , MonadThrow m
     )
  => f Text
  -> f Text
  -> f Text
  -> m Ignores
mkIgnores ignoredFileGlobs ignoredDirGlobs ignoredAbsDirs = do
  ignoresFilesRE   <- fileGlobsToRegex ignoredFileGlobs
  ignoresDirsRE    <- fileGlobsToRegex ignoredDirGlobs
  ignoresAbsDirsRE <- fileGlobsToRegex ignoredAbsDirs
  pure Ignores{ignoresFilesRE, ignoresDirsRE, ignoresAbsDirsRE}

shouldVisit :: Ignores -> AbsDir -> RelDir -> Bool
shouldVisit Ignores{ignoresDirsRE, ignoresAbsDirsRE} (AbsDir absPath) (RelDir relPath) =
  not (reMatchesOsPath ignoresDirsRE relPath) &&
  not (reMatchesOsPath ignoresAbsDirsRE absPath)

isIgnoredFile :: Ignores -> AbsFile -> Bool
isIgnoredFile Ignores{ignoresFilesRE} (AbsFile absPath) =
  reMatchesOsPath ignoresFilesRE absPath

dummyIgnores :: Ignores
dummyIgnores = Ignores
  { ignoresFilesRE   = dummyRe
  , ignoresDirsRE    = dummyRe
  , ignoresAbsDirsRE = dummyRe
  }
  where
    dummyRe :: Regex
    dummyRe = case compileRe "^()$" of
      Nothing -> error "Invalid dummy regex"
      Just x  -> x
