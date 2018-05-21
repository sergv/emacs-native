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
import System.Posix.Files as Posix

import Emacs.Module.Assert

data FollowSymlinks a =
    -- | Recurse into symlinked directories
    FollowSymlinks
  | -- | Do not recurse into symlinked directories, but possibly report them.
    ReportSymlinks (Posix.FileStatus -> Path Abs Dir -> Maybe a)

findRec
  :: forall a. WithCallStack
  => FollowSymlinks a
  -> Int                                            -- ^ Extra search threads to run in parallel.
  -> (Posix.FileStatus -> Path Abs Dir  -> Bool)    -- ^ Whether to visit directory.
  -> (Posix.FileStatus -> Path Abs File -> Maybe a) -- ^ What to do with a file.
  -> (a -> IO ())                                   -- ^ Consume output
  -> Path Abs Dir                                   -- ^ Where to start search.
  -> IO ()
findRec followSymlinks extraJobs dirPred filePred consumeOutput dir = do
  sem <- newNBSem extraJobs
  goDir sem dir
  where
    goDir :: NBSem -> Path Abs Dir -> IO ()
    goDir sem = goDir'
      where
        goDir' :: Path Abs Dir -> IO ()
        goDir' d =
          bracket (Streaming.openDirStream (toFilePath d)) Streaming.closeDirStream (goDirStream d)
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
                  -- ft <- Streaming.getFileType y'
                  let doFile status =
                        for_ (filePred status y'') consumeOutput *> go
                        where
                          y'' :: Path Abs File
                          y'' = Path.Internal.Path y'
                      doDir status =
                        if dirPred status y''
                        then do
                          acquired <- tryAcquireNBSem sem
                          if acquired
                          then
                            withAsync (goDir' y'' `finally` releaseNBSem sem) $ \yAsync ->
                              go *> wait yAsync
                          else goDir' y'' *> go
                        else go
                        where
                          y'' :: Path Abs Dir
                          y'' = Path.Internal.Path y'
                      reportDir status f =
                        for_ (f status y'') consumeOutput *> go
                        where
                          y'' :: Path Abs Dir
                          y'' = Path.Internal.Path y'
                  status <- getSymbolicLinkStatus y'
                  if | isRegularFile  status -> doFile status
                     | isDirectory    status -> doDir status
                     | isSymbolicLink status -> do
                       status' <- try $ getFileStatus y'
                       case status' of
                         Left (_ :: IOException) -> go
                         Right status''
                           | isRegularFile  status'' -> doFile status''
                           | isDirectory    status'' ->
                             case followSymlinks of
                               FollowSymlinks        -> doDir status''
                               ReportSymlinks report -> reportDir status'' report
                           | otherwise               -> go
                     | otherwise            -> go
                  -- case ft of
                  --   Streaming.FTOther        -> go
                  --   Streaming.FTFile         -> doFile
                  --   Streaming.FTFileSym      -> doFile
                  --   Streaming.FTDirectory    -> doDir
                  --   Streaming.FTDirectorySym ->
                  --     case followSymlinks of
                  --       FollowSymlinks        -> doDir
                  --       ReportSymlinks report -> reportDir report
