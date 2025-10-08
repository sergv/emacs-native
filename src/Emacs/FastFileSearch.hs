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
{-# LANGUAGE MonoLocalBinds        #-}
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
import System.Directory.OsPath.Types
import System.OsPath.Types (OsPath)

import Data.Emacs.Module.Args
import Data.Emacs.Module.Doc qualified as Doc
import Emacs.Module
import Emacs.Module.Assert

import Data.Emacs.Path
import Data.Filesystem.Find
import Data.Foldable (traverse_)
import Data.Ignores
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
  => EmacsFunction ('S ('S ('S ('S ('S ('S ('S 'Z))))))) 'Z 'False m v s
emacsFindRec (R roots (R globsToFind (R ignoredFileGlobs (R ignoredDirGlobs (R ignoredDirPrefixes (R ignoredAbsDirs (R isRelativePaths Stop))))))) = do
  roots'                    <- extractListWith extractOsPath roots
  globsToFind'              <- extractListWith extractText globsToFind
  (fileIgnores, dirIgnores) <-
    mkEmacsIgnores ignoredFileGlobs ignoredDirGlobs ignoredDirPrefixes ignoredAbsDirs
  isRelativePaths'          <- extractBool isRelativePaths
  nil'                      <- nil
  globsToFindRE             <- fileGlobsToRegex globsToFind'

  let roots'' :: [AbsDir]
      roots'' = coerce roots'

  results <- liftBase newTMQueueIO

  let shouldCollect :: AbsDir -> AbsFile -> Relative OsPath -> Basename OsPath -> IO (Maybe OsPath)
      shouldCollect _root absPath (Relative relPath) (Basename basePath)
        | isIgnoredFile fileIgnores absPath         = pure Nothing
        | reSetMatchesOsPath globsToFindRE basePath = pure $ Just $ if isRelativePaths' then relPath else unAbsFile absPath
        | otherwise                                 = pure Nothing

      collect :: OsPath -> IO ()
      collect = atomically . writeTMQueue results

      doFind =
        traverse_ collect =<< findRec FollowSymlinks
          (\x y -> not $ isIgnored dirIgnores x y)
          shouldCollect
          roots''

  withAsync (liftBase (doFind `finally` atomically (closeTMQueue results))) $ \searchAsync -> do
    final <- consumeTMQueueWithEarlyTermination
      results
      nil'
      $ \ !acc x -> do
        filepath <- makeString $ BSS.fromShort $ pathForEmacs x
        cons filepath acc
    liftBase $ wait searchAsync
    pure final
