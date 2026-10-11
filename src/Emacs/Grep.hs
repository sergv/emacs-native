----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Grep
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultilineStrings      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Emacs.Grep (initialise) where

import Control.Concurrent.Async.Lifted.Safe
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.Foldable
import Data.Text (Text)
import Data.Tuple.Homogenous
import Prettyprinter (pretty, (<+>))
import System.Directory.OsPath
import System.OsPath (OsPath)
import System.OsPath.Ext

import Control.Monad.EarlyTerminate
import Data.Emacs.Module.Args
import Data.Emacs.Module.Doc qualified as Doc
import Data.Emacs.Path
import Data.Filesystem.Find.Types
import Data.Filesystem.Grep
import Data.Ignores
import Emacs.Ignores (unpackFullIgnores)
import Emacs.Module
import Emacs.Module.Assert
import Emacs.Module.Errors
import Emacs.Module.Monad qualified as Emacs
import Emacs.PredefinedSymbols qualified as Sym

initialise
  :: WithCallStack
  => Emacs.EmacsM s ()
initialise = do
  bindFunction "haskell-native-grep" =<<
    makeFunction emacsGrep emacsGrepDoc
  bindFunction "haskell-native-grep-with-ignores" =<<
    makeFunction emacsGrepWithIgnores emacsGrepWithIgnoresDoc

emacsGrepDoc :: Doc.Doc
emacsGrepDoc =
  "Search for regular expression in files that match some globs and don’t match some others."

emacsGrep
  :: forall m v s.
     ( WithCallStack
     , MonadEmacs m v
     , MonadThrow (m s)
     , MonadEarlyTerminate (m s)
     , MonadIO (m s)
     , MonadBaseControl IO (m s)
     , Forall (Pure (m s))
     )
  => EmacsFunction ('S ('S ('S ('S ('S ('S ('S ('S 'Z)))))))) 'Z 'False m v s
emacsGrep (R roots (R regexp (R globsToFind (R ignoredFileBasenameGlobs (R ignoredFileAbsGlobs (R ignoredDirBasenameGlobs (R ignoredDirAbsGlobs (R ignoreCase Stop)))))))) = do
  roots'       <- extractListWith extractOsPath roots
  regexp'      <- extractByteString regexp
  globsToFind' <- extractListWith extractText globsToFind
  ignoreCase'  <- extractBool ignoreCase
  ignores      <- mkEmacsIgnores ignoredFileBasenameGlobs ignoredFileAbsGlobs ignoredDirBasenameGlobs ignoredDirAbsGlobs

  emacsGrepImpl roots' regexp' globsToFind' ignoreCase' ignores

emacsGrepWithIgnoresDoc :: Doc.Doc
emacsGrepWithIgnoresDoc =
  """
    Search for regular expression in files that match some globs and don’t match some others.

    Similar to ‘haskell-native-grep’ but takes precompiled specification of which files to ignore.
  """

emacsGrepWithIgnores
  :: forall m v s.
     ( WithCallStack
     , MonadEmacs m v
     , MonadThrow (m s)
     , MonadEarlyTerminate (m s)
     , MonadIO (m s)
     , MonadBaseControl IO (m s)
     , Forall (Pure (m s))
     )
  => EmacsFunction ('S ('S ('S ('S ('S 'Z))))) 'Z 'False m v s
emacsGrepWithIgnores (R roots (R regexp (R globsToFind (R ignores (R ignoreCase Stop))))) = do
  roots'       <- extractListWith extractOsPath roots
  regexp'      <- extractByteString regexp
  globsToFind' <- extractListWith extractText globsToFind
  ignoreCase'  <- extractBool ignoreCase
  ignores'     <- unpackFullIgnores ignores

  emacsGrepImpl roots' regexp' globsToFind' ignoreCase' ignores'

emacsGrepImpl
  :: forall m v s.
     ( WithCallStack
     , MonadEmacs m v
     , MonadThrow (m s)
     , MonadEarlyTerminate (m s)
     , MonadIO (m s)
     , MonadBaseControl IO (m s)
     , Forall (Pure (m s))
     )
  => [OsPath]
  -> C8.ByteString
  -> [Text]
  -> Bool
  -> FullIgnores
  -> m s (v s)
emacsGrepImpl roots regexp globsToFind ignoreCase ignores = do
  for_ roots $ \root -> do
    exists <- liftBase $ doesDirectoryExist root
    unless exists $
      throwM $ mkUserError "emacsGrep" $
        "Search root does not exist:" <+> pretty (pathToText root)

  (res, anyMatched) <- grep roots regexp globsToFind ignoreCase ignores $
    \relPath MatchEntry{matchAbsPath, matchLineNum, matchColumnNum, matchLinePrefix, matchLineStr, matchLineSuffix, matchOffset} -> do
      !pathEmacs        <- makeShortByteString $ pathForEmacs $ unAbsFile matchAbsPath
      !shortPathEmacs   <- makeShortByteString relPath
      !matchLineNum'    <- makeInt (fromIntegral matchLineNum)
      !matchColumnNum'  <- makeInt (fromIntegral matchColumnNum)
      !matchLinePrefix' <- makeString' matchLinePrefix
      !matchLineStr'    <- makeString' matchLineStr
      !matchLineSuffix' <- makeString' matchLineSuffix
      !matchOffset'     <- makeInt (fromIntegral matchOffset)
      !emacsMatchStruct <-
        funcallPrimitiveSym
          Sym.makeEgrepMatch
          (Tuple8 (pathEmacs, shortPathEmacs, matchLineNum', matchColumnNum', matchLinePrefix', matchLineStr', matchLineSuffix', matchOffset'))
      pure emacsMatchStruct

  case anyMatched of
    NoFilesMatched   -> intern Sym.noFilesMatched
    SomeFilesMatched -> makeList res

makeString'
  :: MonadEmacs m v
  => C8.ByteString
  -> m s (v s)
makeString' x
  | BS.isValidUtf8 x = makeString x
  | otherwise        = makeBinaryString x
