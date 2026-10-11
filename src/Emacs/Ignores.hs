-- |
-- Module:     Emacs.Ignores
-- Copyright:  (c) Sergey Vinokurov 2026
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE MultilineStrings  #-}
{-# LANGUAGE OverloadedStrings #-}

module Emacs.Ignores
  ( initialise
  , unpackFullIgnores
  ) where

import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Foreign.StablePtr

import Data.Emacs.Module.Args
import Data.Emacs.Module.Doc qualified as Doc
import Data.Emacs.Module.SymbolName.Predefined qualified as Sym
import Data.Ignores
import Emacs.Module
import Emacs.Module.Assert
import Emacs.Module.Errors
import Emacs.Module.Monad qualified as Emacs
import Emacs.PredefinedSymbols qualified as Sym

initialise
  :: WithCallStack
  => Emacs.EmacsM s ()
initialise = do
  bindFunction "haskell-native-full-ignores-p" =<<
    makeFunction emacsFullIgnoresPredicate emacsFullIgnoresPredicateDoc
  bindFunction "haskell-native-make-full-ignores" =<<
    makeFunction emacsMakeFullIgnores emacsMakeFullIgnoresDoc

fullIgnoresEmacsTag
  :: forall m v s. (WithCallStack, MonadEmacs m v)
  => m s (v s)
fullIgnoresEmacsTag = intern Sym.fullIgnores

isFullIgnores
  :: forall m v s. (WithCallStack, MonadEmacs m v)
  => v s -> m s Bool
isFullIgnores x = do
  typ    <- typeOf x
  isCons <- eq typ =<< intern Sym.cons
  if isCons
    then do
      typ' <- car x
      eq typ' =<< fullIgnoresEmacsTag
    else pure False

emacsFullIgnoresPredicate
  :: forall m v s. (WithCallStack, MonadEmacs m v)
  => EmacsFunction ('S 'Z) 'Z 'False m v s
emacsFullIgnoresPredicate (R x Stop) = do
  isIndex <- isFullIgnores x
  if isIndex then intern Sym.t else nil

emacsFullIgnoresPredicateDoc :: Doc.Doc
emacsFullIgnoresPredicateDoc =
  "Check whether an Elisp value is a native full ignores structure."

emacsMakeFullIgnores
  :: forall m v s.
     ( WithCallStack
     , MonadEmacs m v
     , MonadIO (m s)
     , MonadThrow (m s)
     )
  => EmacsFunction ('S ('S ('S ('S 'Z)))) 'Z 'False m v s
emacsMakeFullIgnores (R ignoredFileBasenameGlobs (R ignoredFileAbsGlobs (R ignoredDirBasenameGlobs (R ignoredDirAbsGlobs Stop)))) = do

  ignores <- mkEmacsIgnores
    ignoredFileBasenameGlobs
    ignoredFileAbsGlobs
    ignoredDirBasenameGlobs
    ignoredDirAbsGlobs

  (ptr :: StablePtr FullIgnores) <- liftIO $ newStablePtr ignores

  typ <- fullIgnoresEmacsTag
  cons typ =<< makeUserPtrFromStablePtr ptr

emacsMakeFullIgnoresDoc :: Doc.Doc
emacsMakeFullIgnoresDoc =
  """
    Preprocess globs into ignores for files and directories so that
    subsequent file searches can reuse them.
  """

unpackFullIgnores
  :: forall m v s. (WithCallStack, MonadEmacs m v, MonadBase IO (m s), MonadThrow (m s))
  => v s
  -> m s FullIgnores
unpackFullIgnores x = do
  isIgnores <- isFullIgnores x
  if isIgnores
  then do
    (res :: FullIgnores) <- liftBase . deRefStablePtr =<< extractStablePtrFromUserPtr =<< cdr x
    pure res
  else
    throwM $ mkUserError "unpackFullIgnores" $ "Invalid full ignores argument"
