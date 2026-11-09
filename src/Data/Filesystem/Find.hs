----------------------------------------------------------------------------
-- |
-- Module      :  Data.Filesystem.Find
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE QuantifiedConstraints #-}

#if defined(mingw32_HOST_OS)
#define WINDOWS 1
#endif

module Data.Filesystem.Find
  ( RelativePaths(..)
  , SkipUnreadable(..)
  , fastFileSearch
  , findRec
  , haveReadPermission
  ) where

import Control.Monad.Base
import Control.Monad.Catch (MonadThrow)
import Data.Coerce
import Data.Filesystem.Find.Types
import Data.Ignores
import Data.Regex
import Data.Text (Text)
import Prettyprinter.Generics (Pretty(..), Generic, PPGeneric(..))
import System.Directory.OsPath.Streaming as Streaming
import System.Directory.OsPath.Types
import System.OsPath

#ifndef WINDOWS
import System.Posix.Files.PosixString qualified as PosixString
import System.OsString.Internal.Types (getOsString)
#endif

import Emacs.Module.Assert

data RelativePaths = ProduceRelativePaths | ProduceAbsolutePaths
  deriving (Eq, Ord, Show, Generic)
  deriving Pretty via PPGeneric RelativePaths

data SkipUnreadable = SkipUnreadable | FailOnUnreadable
  deriving (Eq, Ord, Show, Generic)
  deriving Pretty via PPGeneric SkipUnreadable

fastFileSearch
  :: (WithCallStack, Foldable f, Functor f, Coercible a Text, MonadThrow m, MonadBase IO m)
  => RelativePaths
  -> SkipUnreadable
  -> FullIgnores
  -> f a
  -> [AbsDir]
  -> m [OsPath]
fastFileSearch resultPathType skipUnreadableDirs ignores globsToFind roots = do
  globsToFindRE <- fileGlobsToRegex globsToFind
  let
    shouldCollect :: AbsFile -> Relative OsPath -> Basename OsPath -> Maybe OsPath
    shouldCollect absPath (Relative relPath) basename@(Basename basePath)
      | isIgnoredFile ignores basename absPath    = Nothing
      | reSetMatchesOsPath globsToFindRE basePath = Just $
          case resultPathType of
            ProduceRelativePaths -> relPath
            ProduceAbsolutePaths -> unAbsFile absPath
      | otherwise                                 = Nothing
  liftBase $ findRec
    FollowSymlinks
    skipUnreadableDirs
    (\absDir basename -> not $ isIgnoredDir ignores basename absDir)
    (\_root y z w     -> pure $ shouldCollect y z w)
    roots

{-# INLINE findRec #-}
findRec
  :: forall a f. (WithCallStack, Foldable f, Functor f)
  => FollowSymlinks a
  -> SkipUnreadable
  -> (AbsDir -> Basename OsPath -> Bool) -- ^ Whether to visit a directory.
  -> (AbsDir -> AbsFile -> Relative OsPath -> Basename OsPath -> IO (Maybe a))
                                         -- ^ What to do with a file. Receives original directory it was located in.
  -> f AbsDir                            -- ^ Where to start search.
  -> IO [a]
findRec followSymlinks skipUnreadableDirs dirPred filePred roots =
  Streaming.listContentsRecFold
    Nothing
    (\absDir _root _relDir baseDir sym cons descendSubdir rest ->
      let absDir' = AbsDir absDir
      in
        if dirPred absDir' baseDir
        then do
          let descendSubdir' dir =
                case skipUnreadableDirs of
                  SkipUnreadable   -> do
                    isReadable <- haveReadPermission absDir
                    if isReadable
                    then descendSubdir dir
                    else dir
                  FailOnUnreadable -> descendSubdir dir
          case sym of
            Regular -> descendSubdir' rest
            Symlink ->
              case followSymlinks of
                FollowSymlinks        -> descendSubdir' rest
                ReportSymlinks report -> do
                  res <- report absDir' baseDir
                  case res of
                    Nothing -> rest
                    Just x  -> cons x rest
        else
          rest)
    (\absFile root rel baseFile ft ->
      case ft of
        Other _     -> pure Nothing
        Directory _ -> pure Nothing
        File _      -> filePred root (AbsFile absFile) rel baseFile)
    (fmap (coerce addTrailingPathSeparator) roots)

{-# INLINE haveReadPermission #-}
haveReadPermission :: OsPath -> IO Bool
haveReadPermission _path =
#ifdef WINDOWS
  pure True
#endif
#ifndef WINDOWS
  PosixString.fileAccess (getOsString _path) True False False
#endif
