-- |
-- Module:     Data.Ignores
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Ignores
  ( FullIgnores
  , mkEmacsIgnores
  , mkFullIgnores
  , isIgnoredFile
  , isIgnoredDir
  , dummyFullIgnores
  ) where

import Data.Coerce (coerce)
import Data.Filesystem.Find.Types
import Data.Regex
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory.OsPath.Types
import System.OsPath.Types (OsPath)

import Emacs.Module (MonadEmacs, MonadThrow, extractListWith, extractText)
import Emacs.Module.Assert

data FullIgnores = FullIgnores
  { fullIgnoresDirs  :: !Ignores
  , fullIgnoresFiles :: !Ignores
  }

isIgnoredFile :: FullIgnores -> Basename OsPath -> AbsFile -> Bool
isIgnoredFile FullIgnores{fullIgnoresFiles} basename (AbsFile absPath) =
  isIgnored fullIgnoresFiles basename absPath

isIgnoredDir :: FullIgnores -> Basename OsPath -> AbsDir -> Bool
isIgnoredDir FullIgnores{fullIgnoresDirs} basename (AbsDir absPath) =
  isIgnored fullIgnoresDirs basename absPath

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
  -> m s FullIgnores
mkEmacsIgnores ignoredFileBasenameGlobs ignoredFileAbsGlobs ignoredDirBasenameGlobs ignoredDirAbsGlobs = do
  ignoredFileBasenameGlobs' <- extractListWith extractText ignoredFileBasenameGlobs
  ignoredFileAbsGlobs'      <- extractListWith extractText ignoredFileAbsGlobs
  ignoredDirBasenameGlobs'  <- extractListWith extractText ignoredDirBasenameGlobs
  ignoredDirAbsGlobs'       <- extractListWith extractText ignoredDirAbsGlobs
  mkFullIgnores (coerce ignoredFileBasenameGlobs') ignoredFileAbsGlobs' (coerce ignoredDirBasenameGlobs') ignoredDirAbsGlobs'

mkFullIgnores
  :: ( WithCallStack
     , Functor f
     , Foldable f
     , MonadThrow m
     )
  => f (Basename Text)
  -> f Text
  -> f (Basename Text)
  -> f Text
  -> m FullIgnores
mkFullIgnores ignoredFileBasenameGlobs ignoredFileAbsGlobs ignoredDirBasenameGlobs ignoredDirAbsGlobs = do
  fullIgnoresFiles <- mkIgnores ignoredFileBasenameGlobs ignoredFileAbsGlobs
  fullIgnoresDirs  <- mkIgnores (fmap (coerce stripTrailingSlash) ignoredDirBasenameGlobs) (fmap stripTrailingSlash ignoredDirAbsGlobs)
  pure FullIgnores{..}

stripTrailingSlash :: Text -> Text
stripTrailingSlash = T.dropWhileEnd (== '/')

mkIgnores
  :: ( WithCallStack
     , Functor f
     , Foldable f
     , MonadThrow m
     )
  => f (Basename Text)
  -> f Text
  -> m Ignores
mkIgnores ignoredBasenameGlobs ignoredAbsGlobs = do
  ignoresBasenameRE <- fileGlobsToRegex ignoredBasenameGlobs
  ignoresAbsRE      <- fileGlobsToRegex ignoredAbsGlobs
  pure Ignores{ignoresAbsRE, ignoresBasenameRE}

isIgnored :: Ignores -> Basename OsPath -> OsPath -> Bool
isIgnored Ignores{ignoresAbsRE, ignoresBasenameRE} (Basename basePath) absPath =
  reSetMatchesOsPath ignoresBasenameRE basePath ||
    reSetMatchesOsPath ignoresAbsRE absPath

dummyFullIgnores :: FullIgnores
dummyFullIgnores = FullIgnores
  { fullIgnoresDirs  = dummyIgnores
  , fullIgnoresFiles = dummyIgnores
  }

dummyIgnores :: Ignores
dummyIgnores = Ignores
  { ignoresAbsRE      = dummyRe
  , ignoresBasenameRE = dummyRe
  }
  where
    dummyRe :: RegexSet
    dummyRe = case compileReSet ["^()$"] of
      Nothing -> error "internal error: unexpected invalid dummy regex"
      Just x  -> x
