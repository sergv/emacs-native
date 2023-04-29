----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.FastFileSearch
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :   3 May 2018
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Emacs.FastFileSearch (initialise) where

import Control.Concurrent.Async.Lifted.Safe
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Data.ByteString.Short qualified as BSS
import Data.Coerce
import GHC.Conc (getNumCapabilities)

import Data.Emacs.Module.Args
import Data.Emacs.Module.Doc qualified as Doc
import Emacs.Module
import Emacs.Module.Assert

import Data.Emacs.Path
import Data.Filesystem
import Data.Regex
import Emacs.EarlyTermination
import Emacs.Module.Monad qualified as Emacs

initialise
  :: WithCallStack
  => Emacs.EmacsM s ()
initialise = do
  bindFunction "haskell-native-find-rec" =<<
    makeFunction emacsFindRec emacsFindRecDoc

emacsFindRecDoc :: Doc.Doc
emacsFindRecDoc =
  "Recursively find files leveraging multiple cores."

emacsFindRec
  :: forall m v s.
     ( WithCallStack
     , MonadEmacs m v
     , MonadThrow (m s)
     , MonadBaseControl IO (m s)
     , Forall (Pure (m s))
     , forall ss. MonadThrow (m ss)
     )
  => EmacsFunction ('S ('S ('S ('S 'Z)))) 'Z 'False m v s
emacsFindRec (R roots (R globsToFind (R ignoredFileGlobs (R ignoredDirGlobs Stop)))) = do
  roots'            <- extractListWith extractOsPath roots
  globsToFind'      <- extractListWith extractText globsToFind
  ignoredFileGlobs' <- extractListWith extractText ignoredFileGlobs
  ignoredDirGlobs'  <- extractListWith extractText ignoredDirGlobs

  ignoredDirsRE  <- fileGlobsToRegex ignoredDirGlobs'
  ignoredFilesRE <- fileGlobsToRegex ignoredFileGlobs'
  filesToFindRE  <- fileGlobsToRegex globsToFind'

  nil' <- nil
  jobs <- liftBase getNumCapabilities

  let roots'' :: [AbsDir]
      roots'' = coerce roots'

  results <- liftBase newTMQueueIO

  let shouldVisit :: AbsDir -> Bool
      shouldVisit = not . reMatchesOsPath ignoredDirsRE . unAbsDir
      shouldCollect :: AbsDir -> AbsFile -> IO (Maybe AbsFile)
      shouldCollect _root (AbsFile path)
        | reMatchesOsPath ignoredFilesRE path = pure Nothing
        | reMatchesOsPath filesToFindRE path  = pure $ Just $ AbsFile path
        | otherwise                           = pure Nothing

      collect :: AbsFile -> IO ()
      collect = atomically . writeTMQueue results

      doFind =
        findRec FollowSymlinks jobs
          shouldVisit
          shouldCollect
          collect
          roots''

  withAsync (liftBase (doFind `finally` atomically (closeTMQueue results))) $ \searchAsync -> do
    final <- consumeTMQueueWithEarlyTermination
      results
      nil'
      $ \ !acc x -> do
        filepath <- makeString $ BSS.fromShort $ pathForEmacs $ unAbsFile x
        cons filepath acc
    liftBase $ wait searchAsync
    pure final
