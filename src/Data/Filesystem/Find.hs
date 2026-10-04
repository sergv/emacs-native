----------------------------------------------------------------------------
-- |
-- Module      :  Data.Filesystem.Find
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Filesystem.Find
  ( RelativePaths(..)
  , fastFileSearch
  , findRec
  ) where

import Control.Monad.Base
import Control.Monad.Catch (MonadThrow)
import Data.Coerce
import Data.Filesystem.Find.Types
import Data.Ignores
import Data.Regex
import Data.Text (Text)
import System.Directory.OsPath.Streaming as Streaming
import System.Directory.OsPath.Types
import System.OsPath

import Emacs.Module.Assert

data RelativePaths = ProduceRelativePaths | ProduceAbsolutePaths

fastFileSearch
  :: (WithCallStack, Foldable f, Functor f, Coercible a Text, MonadThrow m, MonadBase IO m)
  => RelativePaths
  -> Ignores
  -> Ignores
  -> f a
  -> [AbsDir]
  -> m [OsPath]
fastFileSearch resultPathType fileIgnores dirIgnores globsToFind roots = do
  globsToFindRE <- fileGlobsToRegex globsToFind
  let
    shouldCollect :: AbsDir -> AbsFile -> Relative OsPath -> Basename OsPath -> Maybe OsPath
    shouldCollect _root absPath (Relative relPath) (Basename basePath)
      | isIgnoredFile fileIgnores absPath         = Nothing
      | reSetMatchesOsPath globsToFindRE basePath = Just $ case resultPathType of
        ProduceRelativePaths -> relPath
        ProduceAbsolutePaths -> unAbsFile absPath
      | otherwise                                 = Nothing
  liftBase $ findRec FollowSymlinks
    (\x y -> not $ isIgnored dirIgnores x y)
    (\x y z w -> pure $ shouldCollect x y z w)
    roots

{-# INLINE findRec #-}
findRec
  :: forall a f. (WithCallStack, Foldable f, Functor f)
  => FollowSymlinks a
  -> (OsPath -> Basename OsPath -> Bool) -- ^ Whether to visit a directory.
  -> (AbsDir -> AbsFile -> Relative OsPath -> Basename OsPath -> IO (Maybe a))
                                         -- ^ What to do with a file. Receives original directory it was located in.
  -> f AbsDir                            -- ^ Where to start search.
  -> IO [a]
findRec followSymlinks dirPred filePred roots =
  Streaming.listContentsRecFold
    Nothing
    (\absDir _ _ baseDir sym cons descendSubdir rest ->
      if dirPred absDir baseDir
      then
        case sym of
          Regular -> descendSubdir rest
          Symlink -> case followSymlinks of
            FollowSymlinks        -> descendSubdir rest
            ReportSymlinks report -> do
              res <- report absDir baseDir
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
