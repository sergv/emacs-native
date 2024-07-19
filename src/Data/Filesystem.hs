----------------------------------------------------------------------------
-- |
-- Module      :  Data.Filesystem
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Filesystem
  ( FollowSymlinks(..)
  , findRec
  , AbsDir(..)
  , RelDir(..)
  , AbsFile(..)
  , RelFile(..)
  ) where

import Control.Concurrent.Async
import Control.Monad.Catch
import Data.Coerce
import Data.Foldable
import Data.NBSem
import System.Directory.OsPath.FileType as Streaming
import System.Directory.OsPath.Streaming as Streaming
import System.OsPath

import Emacs.Module.Assert

data FollowSymlinks a =
    -- | Recurse into symlinked directories
    FollowSymlinks
  | -- | Do not recurse into symlinked directories, but possibly report them.
    ReportSymlinks (AbsDir -> RelDir -> IO (Maybe a))

newtype AbsDir  = AbsDir  { unAbsDir  :: OsPath }
newtype RelDir  = RelDir  { unRelDir  :: OsPath }
newtype AbsFile = AbsFile { unAbsFile :: OsPath }
newtype RelFile = RelFile { unRelFile :: OsPath }

{-# INLINE findRec #-}
findRec
  :: forall a f ff. (WithCallStack, Foldable f, Foldable ff)
  => FollowSymlinks a
  -> Int                        -- ^ Extra search threads to run in parallel.
  -> (AbsDir -> RelDir -> Bool) -- ^ Whether to visit a directory.
  -> (AbsDir -> AbsFile -> RelFile -> IO (f a))
                                -- ^ What to do with a file. Receives original directory it was located in.
  -> (a -> IO ())               -- ^ Consume output
  -> ff AbsDir                  -- ^ Where to start search.
  -> IO ()
findRec followSymlinks extraJobs dirPred filePred consumeOutput roots = do
  sem <- newNBSem extraJobs
  let runWithRoot :: AbsDir -> IO () -> IO ()
      runWithRoot currRoot goNext = doDir currRoot (coerce takeFileName currRoot) goNext
        where
          currRootWithTrailingSep :: AbsDir
          currRootWithTrailingSep = AbsDir $ addTrailingPathSeparator $ unAbsDir currRoot

          doDir :: AbsDir -> RelDir -> IO () -> IO ()
          doDir absPath relPath processNextDir
            | dirPred absPath relPath = do
              acquired <- tryAcquireNBSem sem
              if acquired
              then
                withAsync (goDirRelease absPath) $ \yAsync ->
                  processNextDir *> wait yAsync
              else
                goDir absPath *> processNextDir
            | otherwise =
              processNextDir

          goDir :: AbsDir -> IO ()
          goDir d =
            bracket
              (Streaming.openDirStream (unAbsDir d))
              Streaming.closeDirStream
              (goDirStream d)

          goDirRelease :: AbsDir -> IO ()
          goDirRelease d =
            bracket
              (Streaming.openDirStream (unAbsDir d))
              (\stream -> Streaming.closeDirStream stream *> releaseNBSem sem)
              (goDirStream d)

          goDirStream :: AbsDir -> Streaming.DirStream -> IO ()
          goDirStream (AbsDir root) stream = go
            where
              go :: IO ()
              go = do
                x <- Streaming.readDirStream stream
                case x of
                  Nothing -> pure ()
                  Just y  -> do
                    let y' :: OsPath
                        y' = root </> y
                    ft <- Streaming.getFileType y'
                    case ft of
                      Streaming.Other        -> go
                      Streaming.File         -> doFile (AbsFile y') (RelFile y) *> go
                      Streaming.FileSym      -> doFile (AbsFile y') (RelFile y) *> go
                      Streaming.Directory    -> doDir (AbsDir y') (RelDir y) go
                      Streaming.DirectorySym ->
                        case followSymlinks of
                          FollowSymlinks        -> doDir (AbsDir y') (RelDir y) go
                          ReportSymlinks report -> do
                            traverse_ consumeOutput =<< report (AbsDir y') (RelDir y)
                            go

          doFile :: AbsFile -> RelFile -> IO ()
          doFile absPath relPath =
            traverse_ consumeOutput =<< filePred currRootWithTrailingSep absPath relPath

  foldr runWithRoot (pure ()) roots
