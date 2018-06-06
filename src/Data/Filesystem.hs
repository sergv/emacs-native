----------------------------------------------------------------------------
-- |
-- Module      :  Data.Filesystem
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Filesystem
  ( FollowSymlinks(..)
  , findRec
  ) where

import Control.Concurrent.Async
import Control.Exception
import Data.Foldable
import Data.NBSem
import qualified Data.Streaming.Filesystem as Streaming
import Path
import qualified Path.Internal
import qualified System.FilePath as FilePath

import Emacs.Module.Assert

data FollowSymlinks a =
    -- | Recurse into symlinked directories
    FollowSymlinks
  | -- | Do not recurse into symlinked directories, but possibly report them.
    ReportSymlinks (Path Abs Dir -> Maybe a)

findRec
  :: forall a. WithCallStack
  => FollowSymlinks a
  -> Int                        -- ^ Extra search threads to run in parallel.
  -> (Path Abs Dir  -> Bool)    -- ^ Whether to visit directory.
  -> (Path Abs File -> Maybe a) -- ^ What to do with a file.
  -> (a -> IO ())               -- ^ Consume output
  -> [Path Abs Dir]             -- ^ Where to start search.
  -> IO ()
findRec followSymlinks extraJobs dirPred filePred consumeOutput roots = do
  sem <- newNBSem extraJobs
  findWithSem sem $ reverse roots
  where
    findWithSem :: NBSem -> [Path Abs Dir] -> IO ()
    findWithSem _   []       = pure ()
    findWithSem sem (p : ps) =
      foldr doDir (goDir p) ps
      where
        doDir :: Path Abs Dir -> IO () -> IO ()
        doDir path recur =
          if dirPred path
          then do
            acquired <- tryAcquireNBSem sem
            if acquired
            then
              withAsync (goDir path `finally` releaseNBSem sem) $ \yAsync ->
                recur *> wait yAsync
            else goDir path *> recur
          else recur

        goDir :: Path Abs Dir -> IO ()
        goDir d =
          bracket (Streaming.openDirStream (toFilePath d)) Streaming.closeDirStream (goDirStream d)
          where
            goDirStream :: Path Abs Dir -> Streaming.DirStream -> IO ()
            goDirStream root stream = go
              where
                go :: IO ()
                go = do
                  x <- Streaming.readDirStream stream
                  case x of
                    Nothing -> pure ()
                    Just y  -> do
                      let y' :: FilePath
                          y' = toFilePath root FilePath.</> y
                      ft <- Streaming.getFileType y'
                      case ft of
                        Streaming.FTOther        -> go
                        Streaming.FTFile         ->
                          doFile (Path.Internal.Path y') go
                        Streaming.FTFileSym      ->
                          doFile (Path.Internal.Path y') go
                        Streaming.FTDirectory    ->
                          doDir (Path.Internal.Path y') go
                        Streaming.FTDirectorySym ->
                          case followSymlinks of
                            FollowSymlinks        ->
                              doDir (Path.Internal.Path y') go
                            ReportSymlinks report ->
                              reportDir report (Path.Internal.Path y') go

            doFile :: Path Abs File -> IO () -> IO ()
            doFile path recur =
              for_ (filePred path) consumeOutput *> recur
            reportDir :: (Path Abs b -> Maybe a) -> Path Abs b -> IO () -> IO ()
            reportDir f path recur =
              for_ (f path) consumeOutput *> recur
