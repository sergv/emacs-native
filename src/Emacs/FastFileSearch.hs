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
import System.OsPath.Types (OsPath)

import Data.Emacs.Module.Args
import Data.Emacs.Module.Doc qualified as Doc
import Emacs.Module
import Emacs.Module.Assert

import Control.Monad.EarlyTerminate
import Data.Emacs.Path
import Data.Filesystem.Find
import Data.Filesystem.Find.Types
import Data.Foldable (traverse_)
import Data.Ignores
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
     , MonadEarlyTerminate (m s)
     , MonadBaseControl IO (m s)
     , Forall (Pure (m s))
     )
  => EmacsFunction ('S ('S ('S ('S ('S ('S ('S 'Z))))))) 'Z 'False m v s
emacsFindRec (R roots (R globsToFind (R ignoredFileGlobs (R ignoredDirGlobs (R ignoredDirPrefixes (R ignoredAbsDirs (R isRelativePaths Stop))))))) = do
  roots'                    <- extractListWith extractOsPath roots
  globsToFind'              <- extractListWith extractText globsToFind
  (fileIgnores, dirIgnores) <-
    mkEmacsIgnores ignoredFileGlobs ignoredDirGlobs ignoredDirPrefixes ignoredAbsDirs
  isRelativePaths'          <- extractBool isRelativePaths
  nil'                      <- nil

  let roots'' :: [AbsDir]
      roots'' = coerce roots'

  results <- liftBase newTMQueueIO

  let collect :: OsPath -> IO ()
      collect = atomically . writeTMQueue results

      doFind :: IO ()
      doFind =
        traverse_ collect =<< fastFileSearch
          (if isRelativePaths' then ProduceRelativePaths else ProduceAbsolutePaths)
          fileIgnores
          dirIgnores
          globsToFind'
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
