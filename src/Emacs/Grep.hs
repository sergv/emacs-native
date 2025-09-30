----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Grep
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MonoLocalBinds        #-}
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
import Data.Functor.Identity (Identity(runIdentity))
import Data.Tuple.Homogenous
import Prettyprinter (pretty, (<+>))
import System.Directory.OsPath
import System.OsPath.Ext

import Data.Emacs.Module.Args
import Emacs.Module
import Emacs.Module.Assert
import Emacs.Module.Errors

import Data.Emacs.Module.Doc qualified as Doc
import Data.Emacs.Path
import Data.Filesystem.Find
import Data.Filesystem.Grep
import Data.Ignores
import Emacs.Module.Monad qualified as Emacs

initialise
  :: WithCallStack
  => Emacs.EmacsM s ()
initialise =
  bindFunction "haskell-native-grep" =<<
    makeFunction emacsGrepRec emacsGrepRecDoc

emacsGrepRecDoc :: Doc.Doc
emacsGrepRecDoc = runIdentity $ do
  when undefined $ do
    undefined
  pure "Recursively find files leveraging multiple cores."


emacsGrepRec
  :: forall m v s.
     ( WithCallStack
     , MonadEmacs m v
     , MonadIO (m s)
     , MonadBaseControl IO (m s)
     , Forall (Pure (m s))
     , forall ss. MonadThrow (m ss)
     )
  => EmacsFunction ('S ('S ('S ('S ('S ('S ('S ('S 'Z)))))))) 'Z 'False m v s
emacsGrepRec (R roots (R regexp (R globsToFind (R ignoredFileGlobs (R ignoredDirGlobs (R ignoredDirPrefixes (R ignoredAbsDirs (R ignoreCase Stop)))))))) = do
  roots'                    <- extractListWith extractOsPath roots
  regexp'                   <- extractText regexp
  globsToFind'              <- extractListWith extractText globsToFind
  ignoreCase'               <- extractBool ignoreCase
  (fileIgnores, dirIgnores) <-
    mkEmacsIgnores ignoredFileGlobs ignoredDirGlobs ignoredDirPrefixes ignoredAbsDirs

  for_ roots' $ \root -> do
    exists <- liftBase $ doesDirectoryExist root
    unless exists $
      throwM $ mkUserError "emacsGrepRec" $
        "Search root does not exist:" <+> pretty (pathToText root)

  res <- grep roots' regexp' globsToFind' ignoreCase' fileIgnores dirIgnores $
    \relPath MatchEntry{matchAbsPath, matchLineNum, matchColumnNum, matchLinePrefix, matchLineStr, matchLineSuffix, matchOffset} -> do
      !pathEmacs        <- makeShortByteString $ pathForEmacs $ unAbsFile matchAbsPath
      !shortPathEmacs   <- makeShortByteString relPath
      !matchLineNum'    <- makeInt (fromIntegral matchLineNum)
      !matchColumnNum'  <- makeInt (fromIntegral matchColumnNum)
      !matchLinePrefix' <- makeString' matchLinePrefix
      !matchLineStr'    <- makeString' matchLineStr
      !matchLineSuffix' <- makeString' matchLineSuffix
      matchOffset'      <- makeInt (fromIntegral matchOffset)
      !emacsMatchStruct <-
        funcallPrimitiveSym
          "make-egrep-match"
          (Tuple8 (pathEmacs, shortPathEmacs, matchLineNum', matchColumnNum', matchLinePrefix', matchLineStr', matchLineSuffix', matchOffset'))
      pure emacsMatchStruct

  makeList res

makeString'
  :: MonadEmacs m v
  => C8.ByteString
  -> m s (v s)
makeString' x
  | BS.isValidUtf8 x = makeString x
  | otherwise        = makeBinaryString x
