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

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Emacs.FastFileSearch (initialise) where

import Control.Concurrent.Async.Lifted.Safe
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Control.Exception qualified
import Control.Exception.Safe.Checked qualified as Checked
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Text qualified as T
import Prettyprinter
import Data.Traversable
import Data.Vector (Vector)
import Data.Vector.Unboxed qualified as U
import GHC.Conc (getNumCapabilities)

import Data.Emacs.Module.Args
import Data.Emacs.Module.Doc qualified as Doc
import Data.Emacs.Module.SymbolName.TH
import Emacs.Module
import Emacs.Module.Assert
import Emacs.Module.Errors

import Data.Emacs.Path
import Data.Filesystem
import Data.Regex
import Path

initialise
  :: (WithCallStack, Throws EmacsThrow, Throws EmacsError, Throws EmacsInternalError)
  => EmacsM s ()
initialise =
  bindFunction [esym|haskell-native-find-rec|] =<<
    makeFunction emacsFindRec emacsFindRecDoc

emacsFindRecDoc :: Doc.Doc
emacsFindRecDoc = Doc.mkLiteralDoc
  "Recursively find files leveraging multiple cores."#

emacsFindRec
  :: forall m s. (WithCallStack, MonadEmacs m, MonadThrow (m s), MonadBaseControl IO (m s), Forall (Pure (m s)), U.Unbox (EmacsRef m s), Throws UserError)
  => EmacsFunction ('S ('S ('S ('S 'Z)))) 'Z 'False s m
emacsFindRec (R roots (R globsToFind (R ignoredFileGlobs (R ignoredDirGlobs Stop)))) = do
  roots'              <- traverse extractText . U.convert @_ @_ @Vector =<< extractVector roots
  globsToFind'        <- traverse extractText . U.convert @_ @_ @Vector =<< extractVector globsToFind
  ignoredFileGlobs'   <- traverse extractText . U.convert @_ @_ @Vector =<< extractVector ignoredFileGlobs
  ignoredDirGlobs'    <- traverse extractText . U.convert @_ @_ @Vector =<< extractVector ignoredDirGlobs

  jobs <- liftBase getNumCapabilities

  roots'' <- for roots' $ \root ->
    case parseAbsDir $ T.unpack root of
      Nothing -> Checked.throw $ mkUserError "emacsFindRec" $
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
  let rewriteResultsAsEmacsList :: EmacsRef m s -> m s ()
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
    produceRef =<< cdr result
