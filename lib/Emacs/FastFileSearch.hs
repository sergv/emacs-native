----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.FastFileSearch
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :   3 May 2018
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE QuasiQuotes              #-}

module Emacs.FastFileSearch (emacsFindRecDoc, emacsFindRec) where

import Control.Concurrent.Async.Lifted.Safe
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import qualified Control.Exception.Safe.Checked as Checked
import Control.Monad.IO.Class

import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Traversable
import GHC.Conc (getNumCapabilities)

import Data.Emacs.Module.Args
import qualified Data.Emacs.Module.Value as Emacs
import Emacs.Module
import Emacs.Module.Assert
import Emacs.Module.Errors

import Data.Filesystem
import Data.Regex
import Path

emacsFindRecDoc :: C8.ByteString
emacsFindRecDoc =
  "Recursively find files leveraging multiple cores."

emacsFindRec :: WithCallStack => EmacsFunction ('S ('S ('S ('S 'Z)))) 'Z 'False
emacsFindRec env roots globsToFind ignoredFileGlobs ignoredDirGlobs = runEmacsM env $ do
  roots'              <- extractVectorWith extractText roots
  globsToFind'        <- extractVectorWith extractText globsToFind
  ignoredFileGlobs'   <- extractVectorWith extractText ignoredFileGlobs
  ignoredDirGlobs'    <- extractVectorWith extractText ignoredDirGlobs

  jobs <- liftIO getNumCapabilities

  roots'' <- for roots' $ \root ->
    case parseAbsDir $ T.unpack root of
      Nothing -> Checked.throw $ mkUserError "emacsFindRec" $
        "One of the search roots is not a valid absolute directory:" <+> pretty root
      Just x  -> pure x

  results <- liftIO newTMQueueIO

  ignoredDirsRE  <- globsToRegex ignoredDirGlobs'
  ignoredFilesRE <- globsToRegex ignoredFileGlobs'
  filesToFindRE  <- globsToRegex globsToFind'
  let shouldVisit :: Path Abs Dir -> Bool
      shouldVisit = not . reMatchesPath ignoredDirsRE
      shouldCollect :: Path Abs File -> Maybe (Path Abs File)
      shouldCollect path
        | reMatchesPath ignoredFilesRE path = Nothing
        | reMatchesPath filesToFindRE path  = Just path
        | otherwise                         = Nothing

      collect :: Path Abs File -> IO ()
      collect = atomically . writeTMQueue results

  nil'   <- nil
  result <- cons nil' nil'
  let rewriteResultsAsEmacsList :: Emacs.Value -> EmacsM ()
      rewriteResultsAsEmacsList resultList = do
        res <- liftIO $ atomically $ readTMQueue results
        case res of
          Nothing -> pure ()
          Just x  -> do
            filepath    <- makeText $ T.pack $ toFilePath x
            resultList' <- cons filepath nil'
            _           <- setcdr resultList resultList'
            rewriteResultsAsEmacsList resultList'

  withAsync (rewriteResultsAsEmacsList result) $ \rewriteAsync -> do
    liftIO $ findRec FollowSymlinks jobs
      shouldVisit
      shouldCollect
      collect
      roots''
    liftIO $ atomically $ closeTMQueue results
    wait rewriteAsync
    cdr result
