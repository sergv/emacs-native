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
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Emacs.FastFileSearch (initialise) where

import Control.Concurrent.Async.Lifted.Safe
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import qualified Control.Exception.Safe.Checked as Checked
import Control.Monad.Base
import Control.Monad.Trans.Control

import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Traversable
import GHC.Conc (getNumCapabilities)

import Data.Emacs.Module.Args
import Data.Emacs.Module.SymbolName.TH
import Emacs.Module
import Emacs.Module.Assert
import Emacs.Module.Errors

import Data.Filesystem
import Data.Regex
import Path

initialise
  :: (WithCallStack, Throws EmacsThrow, Throws EmacsError, Throws EmacsInternalError)
  => EmacsM s ()
initialise =
  bindFunction [esym|haskell-native-find-rec|] =<<
    makeFunction emacsFindRec emacsFindRecDoc

emacsFindRecDoc :: C8.ByteString
emacsFindRecDoc =
  "Recursively find files leveraging multiple cores."

emacsFindRec
  :: forall m s. (WithCallStack, MonadEmacs m, Monad (m s), MonadThrow (m s), MonadBaseControl IO (m s), Forall (Pure (m s)))
  => EmacsFunction ('S ('S ('S ('S 'Z)))) 'Z 'False s m
emacsFindRec (R roots (R globsToFind (R ignoredFileGlobs (R ignoredDirGlobs Stop)))) = do
  roots'              <- extractVectorWith extractText roots
  globsToFind'        <- extractVectorWith extractText globsToFind
  ignoredFileGlobs'   <- extractVectorWith extractText ignoredFileGlobs
  ignoredDirGlobs'    <- extractVectorWith extractText ignoredDirGlobs

  jobs <- liftBase getNumCapabilities

  roots'' <- for roots' $ \root ->
    case parseAbsDir $ T.unpack root of
      Nothing -> Checked.throw $ mkUserError "emacsFindRec" $
        "One of the search roots is not a valid absolute directory:" <+> pretty root
      Just x  -> pure x

  results <- liftBase newTMQueueIO

  ignoredDirsRE  <- globsToRegex ignoredDirGlobs'
  ignoredFilesRE <- globsToRegex ignoredFileGlobs'
  filesToFindRE  <- globsToRegex globsToFind'
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
  let rewriteResultsAsEmacsList :: EmacsRef m s -> m s ()
      rewriteResultsAsEmacsList resultList = do
        res <- liftBase $ atomically $ readTMQueue results
        case res of
          Nothing -> pure ()
          Just x  -> do
            filepath    <- makeText $ T.pack $ toFilePath x
            resultList' <- cons filepath nil'
            _           <- setcdr resultList resultList'
            rewriteResultsAsEmacsList resultList'

      doFind =
        findRec FollowSymlinks jobs
          shouldVisit
          shouldCollect
          collect
          roots''

  withAsync (liftBase (doFind *> atomically (closeTMQueue results))) $ \searchAsync -> do
    rewriteResultsAsEmacsList result
    liftBase (wait searchAsync)
    produceRef =<< cdr result
