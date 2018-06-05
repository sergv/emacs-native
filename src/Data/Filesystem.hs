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
  -> Int                                            -- ^ Extra search threads to run in parallel.
  -> (Path Abs Dir  -> Bool)    -- ^ Whether to visit directory.
  -> (Path Abs File -> Maybe a) -- ^ What to do with a file.
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
                  let doFile =
                        for_ (filePred y'') consumeOutput *> go
                        where
                          y'' :: Path Abs File
                          y'' = Path.Internal.Path y'
                      doDir =
                        if dirPred y''
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
                      reportDir f =
                        for_ (f y'') consumeOutput *> go
                        where
                          y'' :: Path Abs Dir
                          y'' = Path.Internal.Path y'
                  ft <- Streaming.getFileType y'
                  case ft of
                    Streaming.FTOther        -> go
                    Streaming.FTFile         -> doFile
                    Streaming.FTFileSym      -> doFile
                    Streaming.FTDirectory    -> doDir
                    Streaming.FTDirectorySym ->
                      case followSymlinks of
                        FollowSymlinks        -> doDir
                        ReportSymlinks report -> reportDir report
