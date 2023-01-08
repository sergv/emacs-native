----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.FastFileSearch
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :   3 May 2018
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Emacs.FastFileSearch (initialise) where

import Control.Concurrent.Async.Lifted.Safe
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Control.Exception qualified
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Text qualified as T
import Prettyprinter
import Data.Traversable
import GHC.Conc (getNumCapabilities)

import Data.Emacs.Module.Args
import Data.Emacs.Module.Doc qualified as Doc
import Emacs.Module
import Emacs.Module.Assert
import Emacs.Module.Errors

import Data.Emacs.Path
import Data.Filesystem
import Data.Regex
import Emacs.Module.Monad qualified as Emacs
import Path

initialise
  :: WithCallStack
  => Emacs.EmacsM s ()
initialise =
  bindFunction "haskell-native-find-rec" =<<
    makeFunction emacsFindRec emacsFindRecDoc

emacsFindRecDoc :: Doc.Doc
emacsFindRecDoc =
  "Recursively find files leveraging multiple cores."

emacsFindRec
  :: forall m v s. (WithCallStack, MonadEmacs m v, MonadThrow (m s), MonadBaseControl IO (m s), Forall (Pure (m s)))
  => EmacsFunction ('S ('S ('S ('S 'Z)))) 'Z 'False m v s
emacsFindRec (R roots (R globsToFind (R ignoredFileGlobs (R ignoredDirGlobs Stop)))) = do
  roots'            <- extractListWith extractText roots
  globsToFind'      <- extractListWith extractText globsToFind
  ignoredFileGlobs' <- extractListWith extractText ignoredFileGlobs
  ignoredDirGlobs'  <- extractListWith extractText ignoredDirGlobs

  jobs <- liftBase getNumCapabilities

  roots'' <- for roots' $ \root ->
    case parseAbsDir $ T.unpack root of
      Nothing -> throwM $ mkUserError "emacsFindRec" $
        "One of the search roots is not a valid absolute directory:" <+> pretty root
      Just x  -> pure x

  results <- liftBase newTMQueueIO

  ignoredDirsRE  <- fileGlobsToRegex ignoredDirGlobs'
  ignoredFilesRE <- fileGlobsToRegex ignoredFileGlobs'
  filesToFindRE  <- fileGlobsToRegex globsToFind'
  let shouldVisit :: Path Abs Dir -> Bool
      shouldVisit = not . reMatchesPath ignoredDirsRE
      shouldCollect :: Path Abs Dir -> Path Abs File -> IO (Maybe (Path Abs File))
      shouldCollect _root path
        | reMatchesPath ignoredFilesRE path = pure Nothing
        | reMatchesPath filesToFindRE path  = pure $ Just path
        | otherwise                         = pure Nothing

      collect :: Path Abs File -> IO ()
      collect = atomically . writeTMQueue results

  nil'   <- nil
  result <- cons nil' nil'
  let rewriteResultsAsEmacsList :: v s -> m s ()
      rewriteResultsAsEmacsList resultList = do
        res <- liftBase $ atomically $ readTMQueue results
        case res of
          Nothing -> pure ()
          Just x  -> do
            filepath    <- makeString $ pathForEmacs x
            resultList' <- cons filepath nil'
            _           <- setcdr resultList resultList'
            rewriteResultsAsEmacsList resultList'

      doFind =
        findRec FollowSymlinks jobs
          shouldVisit
          shouldCollect
          collect
          roots''

  withAsync (liftBase (doFind `Control.Exception.finally` atomically (closeTMQueue results))) $ \searchAsync -> do
    rewriteResultsAsEmacsList result
    liftBase (wait searchAsync)
    cdr result
