----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Trie
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Emacs.EprojTagIndex (initialise) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.ByteString.Short (ShortByteString)
import Data.IORef
import Data.Traversable
import Data.Tuple.Homogenous
import Foreign.StablePtr
import Prettyprinter (Pretty(..), (<+>))

import Data.Eproj (EprojTagIndex, EprojTag(..), EprojTagProps(..))
import Data.Eproj qualified as Eproj

import Data.Emacs.Module.Args
import Data.Emacs.Module.Doc qualified as Doc
import Data.Emacs.Module.SymbolName
import Data.Regex
import Emacs.Module
import Emacs.Module.Assert
import Emacs.Module.Errors
import Emacs.Module.Monad qualified as Emacs

initialise
  :: WithCallStack
  => Emacs.EmacsM s ()
initialise = do
  bindFunction "haskell-native--eproj-tag-index-p" =<<
    makeFunction emacsEprojTagIndex emacsIsEprojTagIndexDoc
  bindFunction "haskell-native--eproj-tag-index-empty" =<<
    makeFunction emacsEmptyEprojTagIndex emacsEmptyEprojTagIndexDoc
  bindFunction "haskell-native--eproj-tag-index-size" =<<
    makeFunction emacsEprojTagIndexSize emacsEprojTagIndexSizeDoc
  bindFunction "haskell-native--eproj-tag-index-add!" =<<
    makeFunction emacsEprojTagIndexAdd emacsEprojTagIndexAddDoc
  bindFunction "haskell-native--eproj-tag-index-lookup" =<<
    makeFunction emacsEprojTagIndexLookup emacsEprojTagIndexLookupDoc
  bindFunction "haskell-native--eproj-tag-index-entries-matching-re" =<<
    makeFunction emacsEprojTagIndexEntriesMatchinRE emacsEprojTagIndexEntriesMatchinREDoc
  bindFunction "haskell-native--eproj-tag-index-keys" =<<
    makeFunction emacsEprojTagIndexKeys emacsEprojTagIndexKeysDoc
  bindFunction "haskell-native--eproj-tag-index-entries" =<<
    makeFunction emacsEprojTagIndexEntries emacsEprojTagIndexEntriesDoc
  bindFunction "haskell-native--eproj-tag-index-drop-tags-from-file!" =<<
    makeFunction emacsEprojTagIndexDropTagsFromFile emacsEprojTagIndexDropTagsFromFileDoc
  bindFunction "haskell-native--eproj-tag-index-map-union!" =<<
    makeFunction emacsEprojTagIndexMapUnion emacsEprojTagIndexUnionDoc

eprojTagIndexType
  :: forall m v s. (WithCallStack, MonadEmacs m v)
  => m s (v s)
eprojTagIndexType = intern "haskell-native--eproj-tag-index"

isEprojTagIndex
  :: forall m v s. (WithCallStack, MonadEmacs m v)
  => v s -> m s Bool
isEprojTagIndex x = do
  typ    <- typeOf x
  isCons <- eq typ =<< intern "cons"
  if isCons
    then do
      typ' <- car x
      eq typ' =<< eprojTagIndexType
    else pure False

unpackEmacsEprojTagIndex
  :: forall m v s. (WithCallStack, MonadEmacs m v, MonadIO (m s), MonadThrow (m s))
  => v s -> m s EmacsEprojTagIndex
unpackEmacsEprojTagIndex x = do
  isTrie <- isEprojTagIndex x
  if isTrie
    then do
      (res :: EmacsEprojTagIndex) <- liftIO . deRefStablePtr =<< extractStablePtrFromUserPtr =<< cdr x
      pure res
    else do
      let str = "Prettyprinting temporarily omitted" :: String
      throwM $ mkUserError "unpackEmacsEprojTagIndex" $ "Invalid native eproj tag index:" <+> pretty str

unpackEprojTagIndex
  :: forall m v s. (WithCallStack, MonadEmacs m v, MonadIO (m s), MonadThrow (m s))
  => v s -> m s EprojTagIndex
unpackEprojTagIndex =
  liftIO . readIORef . unEmacsEprojTagIndex <=< unpackEmacsEprojTagIndex


newtype EmacsEprojTagIndex = EmacsEprojTagIndex
  { unEmacsEprojTagIndex :: IORef EprojTagIndex }

emacsEprojTagIndex
  :: forall m v s. (WithCallStack, MonadEmacs m v)
  => EmacsFunction ('S 'Z) 'Z 'False m v s
emacsEprojTagIndex (R x Stop) = do
  isIndex <- isEprojTagIndex x
  if isIndex then intern "t" else nil

emacsIsEprojTagIndexDoc :: Doc.Doc
emacsIsEprojTagIndexDoc =
  "Check whether an Elisp value is a native eproj tag index."

emacsEmptyEprojTagIndex
  :: forall m v s. (WithCallStack, MonadEmacs m v, MonadIO (m s))
  => EmacsFunction 'Z 'Z 'False m v s
emacsEmptyEprojTagIndex Stop = do
  typ <- eprojTagIndexType
  (radixTreePtr :: StablePtr EmacsEprojTagIndex) <- liftIO $ do
    index <- newIORef Eproj.empty
    newStablePtr $ EmacsEprojTagIndex index
  payload <- makeUserPtrFromStablePtr radixTreePtr
  cons typ payload

emacsEmptyEprojTagIndexDoc :: Doc.Doc
emacsEmptyEprojTagIndexDoc =
  "Create an empty native eproj tag index."

emacsEprojTagIndexSize
  :: forall m v s. (WithCallStack, MonadEmacs m v, MonadIO (m s), MonadThrow (m s))
  => EmacsFunction ('S 'Z) 'Z 'False m v s
emacsEprojTagIndexSize (R index Stop) = do
  index' <- unpackEprojTagIndex index
  makeInt (Eproj.size index')

emacsEprojTagIndexSizeDoc :: Doc.Doc
emacsEprojTagIndexSizeDoc =
  "Get number of elements in a native eproj tag index."

emacsEprojTagIndexAdd
  :: forall m v s. (WithCallStack, MonadEmacs m v, MonadIO (m s), MonadThrow (m s), MonadCatch (m s))
  => EmacsFunction ('S ('S ('S ('S ('S 'Z))))) 'Z 'False m v s
emacsEprojTagIndexAdd (R sym (R file (R line (R props (R index Stop))))) = do
  index'  <- unpackEmacsEprojTagIndex index
  sym'    <- extractShortByteString sym
  etFile  <- extractShortByteString file
  etLine  <- extractInt line
  etProps <- (\f -> foldlEmacsListWith f Nil props) $ \tl consCell -> do
    car' <- extractShortByteString =<< symbolName =<< car consCell
    cdr' <- extractShortByteString =<< cdr consCell
    pure $ Cons car' cdr' tl
  let tag = EprojTag{etFile, etLine, etProps}
  liftIO $ atomicModifyIORef (unEmacsEprojTagIndex index') $ \index'' ->
    (Eproj.insert sym' tag index'', ())
  nil

emacsEprojTagIndexAddDoc :: Doc.Doc
emacsEprojTagIndexAddDoc =
  "Add an item to a native eproj tag index."

emacsEprojTagIndexLookup
  :: forall m v s. (WithCallStack, MonadEmacs m v, MonadIO (m s), MonadThrow (m s), MonadCatch (m s))
  => EmacsFunction ('S ('S 'Z)) ('S 'Z) 'False m v s
emacsEprojTagIndexLookup (R key (R index (O def Stop))) = do
  index' <- unpackEprojTagIndex index
  key'   <- extractShortByteString key
  case Eproj.lookup key' index' of
    Nothing -> maybe nil pure def
    Just x  -> makeList =<< traverse eprojTagToEmacs x

emacsEprojTagIndexLookupDoc :: Doc.Doc
emacsEprojTagIndexLookupDoc =
  "Lookup a tag within a native eproj tag index."

tagPropsToEmacs
  :: MonadEmacs m v
  => EprojTagProps
  -> m s (v s)
tagPropsToEmacs = unfoldEmacsListWith $ \case
  Nil         -> pure Nothing
  Cons x y rest -> do
    x' <- intern $ mkSymbolNameShortByteString x
    y' <- makeShortByteString y
    z  <- cons x' y'
    pure $ Just (z, rest)

eprojTagToEmacs
  :: MonadEmacs m v
  => EprojTag ShortByteString
  -> m s (v s)
eprojTagToEmacs EprojTag{etFile, etLine, etProps} = do
  etFile'  <- makeShortByteString etFile
  etLine'  <- makeInt etLine
  etProps' <- tagPropsToEmacs etProps
  funcallPrimitiveSym "make-eproj-tag" (Tuple3 (etFile', etLine', etProps'))

emacsEprojTagIndexEntriesMatchinRE
  :: forall m v s. (WithCallStack, MonadEmacs m v, MonadIO (m s), MonadThrow (m s), MonadCatch (m s))
  => EmacsFunction ('S ('S ('S 'Z))) 'Z 'False m v s
emacsEprojTagIndexEntriesMatchinRE (R regexp (R index (R ignoreCase Stop))) = do
  index'      <- unpackEprojTagIndex index
  ignoreCase' <- extractBool ignoreCase

  let compOpts =
        defaultCompOpt
          { multiline      = False
          , caseSensitive  = not ignoreCase'
          , lastStarGreedy = True
          }
  regexp' <- compileReWithOpts compOpts =<< extractText regexp

  makeList =<< traverse eprojTagToEmacs (Eproj.keysMatchingRegexp regexp' index')

emacsEprojTagIndexEntriesMatchinREDoc :: Doc.Doc
emacsEprojTagIndexEntriesMatchinREDoc =
  "Get all tag entries whose key matches a regexp."

emacsEprojTagIndexKeys
  :: forall m v s. (WithCallStack, MonadEmacs m v, MonadIO (m s), MonadThrow (m s), MonadCatch (m s))
  => EmacsFunction ('S 'Z) 'Z 'False m v s
emacsEprojTagIndexKeys (R index Stop) = do
  index' <- unpackEprojTagIndex index
  makeList =<< traverse makeShortByteString (Eproj.keys index')

emacsEprojTagIndexKeysDoc :: Doc.Doc
emacsEprojTagIndexKeysDoc =
  "Get all keys of a native eproj tag index."

emacsEprojTagIndexEntries
  :: forall m v s. (WithCallStack, MonadEmacs m v, MonadIO (m s), MonadThrow (m s), MonadCatch (m s))
  => EmacsFunction ('S 'Z) 'Z 'False m v s
emacsEprojTagIndexEntries (R index Stop) = do
  index' <- unpackEprojTagIndex index
  items  <- for (Eproj.toAscList index') $ \(key, vals) -> do
    key' <- makeShortByteString key
    cons key' =<< makeList =<< traverse eprojTagToEmacs vals
  makeList items

emacsEprojTagIndexEntriesDoc :: Doc.Doc
emacsEprojTagIndexEntriesDoc =
  "Get all entries from a native eproj tag index in the form of (string . tags)."

emacsEprojTagIndexDropTagsFromFile
  :: forall m v s. (WithCallStack, MonadEmacs m v, MonadIO (m s), MonadThrow (m s), MonadCatch (m s))
  => EmacsFunction ('S ('S 'Z)) 'Z 'False m v s
emacsEprojTagIndexDropTagsFromFile (R file (R index Stop)) = do
  file'  <- extractShortByteString file
  index' <- unpackEmacsEprojTagIndex index
  liftIO $ atomicModifyIORef (unEmacsEprojTagIndex index') $ \index'' ->
    (Eproj.dropTagsFromFile file' index'', ())
  nil

emacsEprojTagIndexDropTagsFromFileDoc :: Doc.Doc
emacsEprojTagIndexDropTagsFromFileDoc =
  "Remove all tags that come from the specified file."

emacsEprojTagIndexMapUnion
  :: forall m v s. (WithCallStack, MonadEmacs m v, MonadIO (m s), MonadThrow (m s), MonadCatch (m s))
  => EmacsFunction ('S ('S 'Z)) 'Z 'False m v s
emacsEprojTagIndexMapUnion (R index (R auxIndex Stop)) = do
  index'    <- unpackEmacsEprojTagIndex index
  auxIndex' <- unpackEprojTagIndex auxIndex
  liftIO $ atomicModifyIORef (unEmacsEprojTagIndex index') $ \index'' ->
    (Eproj.union index'' auxIndex', ())
  nil

emacsEprojTagIndexUnionDoc :: Doc.Doc
emacsEprojTagIndexUnionDoc =
  "Destructively update first tag index with all elements of the second tag index."
