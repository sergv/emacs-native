----------------------------------------------------------------------------
-- |
-- Module      :  Data.Filesystem.Find
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DerivingVia #-}

module Data.Filesystem.Find
  ( FollowSymlinks(..)
  , findRec
  , AbsDir(..)
  , RelDir(..)
  , AbsFile(..)
  , RelFile(..)
  ) where

import Data.Coerce
import Data.Foldable
import Prettyprinter.Show
import System.Directory.OsPath.Streaming as Streaming
import System.OsPath

import Emacs.Module.Assert

data FollowSymlinks a =
    -- | Recurse into symlinked directories
    FollowSymlinks
  | -- | Do not recurse into symlinked directories, but possibly report them.
    ReportSymlinks (AbsDir -> RelDir -> IO (Maybe a))

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

{-# INLINE findRec #-}
findRec
  :: forall a f. (WithCallStack, Foldable f, Functor f)
  => FollowSymlinks a
  -> Int                        -- ^ Extra search threads to run in parallel.
  -> (AbsDir -> RelDir -> Bool) -- ^ Whether to visit a directory.
  -> (AbsDir -> AbsFile -> RelFile -> IO (Maybe a))
                                -- ^ What to do with a file. Receives original directory it was located in.
  -> (a -> IO ())               -- ^ Consume output
  -> f AbsDir                  -- ^ Where to start search.
  -> IO ()
findRec followSymlinks _extraJobs dirPred filePred consumeOutput roots =
  traverse_ consumeOutput =<< Streaming.listContentsRecFold
    Nothing
    (\absDir _ _ (Basename baseDir) sym cons descendSubdir rest ->
      if dirPred (AbsDir absDir) (RelDir baseDir)
      then
        case sym of
          Regular -> descendSubdir rest
          Symlink -> case followSymlinks of
            FollowSymlinks        -> descendSubdir rest
            ReportSymlinks report -> do
              res <- report (AbsDir absDir) (RelDir baseDir)
              case res of
                Nothing -> rest
                Just x  -> cons x rest
      else
        rest)
    (\absFile root _ (Basename baseFile) ft ->
      case ft of
        Other _     -> pure Nothing
        Directory _ -> pure Nothing
        File _      -> filePred root (AbsFile absFile) (RelFile baseFile))
    (fmap (coerce addTrailingPathSeparator) roots)
