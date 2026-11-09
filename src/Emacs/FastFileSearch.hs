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
{-# LANGUAGE MultilineStrings      #-}
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
import Data.Foldable (traverse_)
import Data.Text (Text)
import System.OsPath.Types (OsPath)

import Control.Monad.EarlyTerminate
import Data.Emacs.Module.Args
import Data.Emacs.Module.Doc qualified as Doc
import Data.Emacs.Path
import Data.Filesystem.Find
import Data.Filesystem.Find.Types
import Data.Ignores
import Emacs.EarlyTermination
import Emacs.Ignores (unpackFullIgnores)
import Emacs.Module
import Emacs.Module.Assert
import Emacs.Module.Monad qualified as Emacs

initialise
  :: WithCallStack
  => Emacs.EmacsM s ()
initialise = do
  bindFunction "haskell-native-find-rec" =<<
    makeFunction emacsFindRec emacsFindRecDoc
  bindFunction "haskell-native-find-rec-with-ignores" =<<
    makeFunction emacsFindRecWithIgnores emacsFindRecWithIgnoresDoc

emacsFindRecDoc :: Doc.Doc
emacsFindRecDoc =
  "Recursively search for files matching globs across directories."

emacsFindRec
  :: forall m v s.
     ( WithCallStack
     , MonadEmacs m v
     , MonadThrow (m s)
     , MonadEarlyTerminate (m s)
     , MonadBaseControl IO (m s)
     , Forall (Pure (m s))
     )
  => EmacsFunction ('S ('S ('S ('S ('S ('S ('S ('S 'Z)))))))) 'Z 'False m v s
emacsFindRec (R roots (R globsToFind (R ignoredFileBasenameGlobs (R ignoredFileAbsGlobs (R ignoredDirBasenameGlobs (R ignoredDirAbsGlobs (R isRelativePaths (R isSkipUnreadable Stop)))))))) = do
  roots'                <- extractListWith extractOsPath roots
  globsToFind'          <- extractListWith extractText globsToFind
  ignores               <- mkEmacsIgnores ignoredFileBasenameGlobs ignoredFileAbsGlobs ignoredDirBasenameGlobs ignoredDirAbsGlobs
  isRelativePaths'      <- extractBool isRelativePaths
  isSkipUnreadable' <- extractBool isSkipUnreadable

  emacsFindRecImpl
    roots'
    globsToFind'
    ignores
    (if isRelativePaths' then ProduceRelativePaths else ProduceAbsolutePaths)
    (if isSkipUnreadable' then SkipUnreadable else FailOnUnreadable)

emacsFindRecWithIgnoresDoc :: Doc.Doc
emacsFindRecWithIgnoresDoc =
  """
    Recursively search for files matching globs across directories.

    Similar to ‘haskell-native-find-rec’ but takes precompiled specification of which files to ignore.
  """

emacsFindRecWithIgnores
  :: forall m v s.
     ( WithCallStack
     , MonadEmacs m v
     , MonadThrow (m s)
     , MonadEarlyTerminate (m s)
     , MonadBaseControl IO (m s)
     , Forall (Pure (m s))
     )
  => EmacsFunction ('S ('S ('S ('S ('S 'Z))))) 'Z 'False m v s
emacsFindRecWithIgnores (R roots (R globsToFind (R ignores (R isRelativePaths (R isSkipUnreadable Stop))))) = do
  roots'                <- extractListWith extractOsPath roots
  globsToFind'          <- extractListWith extractText globsToFind
  ignores'              <- unpackFullIgnores ignores
  isRelativePaths'      <- extractBool isRelativePaths
  isSkipUnreadable' <- extractBool isSkipUnreadable

  emacsFindRecImpl
    roots'
    globsToFind'
    ignores'
    (if isRelativePaths' then ProduceRelativePaths else ProduceAbsolutePaths)
    (if isSkipUnreadable' then SkipUnreadable else FailOnUnreadable)

emacsFindRecImpl
  :: forall m v s.
     ( WithCallStack
     , MonadEmacs m v
     , MonadThrow (m s)
     , MonadEarlyTerminate (m s)
     , MonadBaseControl IO (m s)
     , Forall (Pure (m s))
     )
  => [OsPath]
  -> [Text]
  -> FullIgnores
  -> RelativePaths
  -> SkipUnreadable
  -> m s (v s)
emacsFindRecImpl roots' globsToFind' ignores isRelativePaths' skipUnreadable = do
  nil'             <- nil

  let roots'' :: [AbsDir]
      roots'' = coerce roots'

  results <- liftBase newTMQueueIO

  let collect :: OsPath -> IO ()
      collect = atomically . writeTMQueue results

      doFind :: IO ()
      doFind =
        traverse_ collect =<< fastFileSearch
          isRelativePaths'
          skipUnreadable
          ignores
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
