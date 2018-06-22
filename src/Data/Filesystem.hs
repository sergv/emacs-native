----------------------------------------------------------------------------
-- |
-- Module      :  Data.Filesystem
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Filesystem
  ( FollowSymlinks(..)
  , findRec
  ) where

import Control.Concurrent.Async.Lifted.Safe
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
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

newtype FindEnv = FindEnv { feRoot :: Path Abs Dir }

findRec
  :: forall a f ff. (WithCallStack, Foldable f, Foldable ff)
  => FollowSymlinks a
  -> Int                     -- ^ Extra search threads to run in parallel.
  -> (Path Abs Dir  -> Bool) -- ^ Whether to visit a directory.
  -> (Path Abs Dir -> Path Abs File -> IO (f a))
                             -- ^ What to do with a file.
  -> (a -> IO ())            -- ^ Consume output
  -> ff (Path Abs Dir)       -- ^ Where to start search.
  -> IO ()
findRec followSymlinks extraJobs dirPred filePred consumeOutput roots = do
  sem <- newNBSem extraJobs
  findWithSem sem $ reverse $ toList roots
  where
    findWithSem :: NBSem -> [Path Abs Dir] -> IO ()
    findWithSem _   []       = pure ()
    findWithSem sem (p : ps) =
      foldr runWithRoot base ps
      where
        base :: IO ()
        base = runReaderT (goDir p) (FindEnv p)

        runWithRoot :: Path Abs Dir -> IO () -> IO ()
        runWithRoot p' goNext =
          runReaderT (doDir p' (liftBase goNext)) (FindEnv p')

        doDir
          :: (MonadReader FindEnv m, MonadBaseControl IO m, MonadMask m, Forall (Pure m))
          => Path Abs Dir -> m () -> m ()
        doDir path processNextDir
          | dirPred path = do
            acquired <- liftBase $ tryAcquireNBSem sem
            if acquired
            then
              withAsync (goDir path `finally` liftBase (releaseNBSem sem)) $ \yAsync ->
                processNextDir *> wait yAsync
            else goDir path *> processNextDir
          | otherwise =
            processNextDir

        goDir
          :: forall m. (MonadReader FindEnv m, MonadBaseControl IO m, MonadMask m, Forall (Pure m))
          => Path Abs Dir -> m ()
        goDir d =
          bracket
            (liftBase (Streaming.openDirStream (toFilePath d)))
            (liftBase . Streaming.closeDirStream)
            (goDirStream d)
          where
            goDirStream :: Path Abs Dir -> Streaming.DirStream -> m ()
            goDirStream root stream = go
              where
                go :: m ()
                go = do
                  x <- liftBase $ Streaming.readDirStream stream
                  case x of
                    Nothing -> pure ()
                    Just y  -> do
                      let y' :: FilePath
                          y' = toFilePath root FilePath.</> y
                      ft <- liftBase $ Streaming.getFileType y'
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

            doFile :: Path Abs File -> m () -> m ()
            doFile path recur = do
              root <- asks feRoot
              liftBase (traverse_ consumeOutput =<< filePred root path)
              recur
            reportDir :: (Path Abs b -> Maybe a) -> Path Abs b -> m () -> m ()
            reportDir f path recur =
              liftBase (for_ (f path) consumeOutput) *> recur
