----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Trie
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Emacs.EprojTagIndex (initialise) where

import qualified Control.Exception.Safe.Checked as Checked
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Short (ShortByteString)
import Data.IORef
import Data.Text.Prettyprint.Doc (Pretty(..), (<+>))
import Data.Traversable
import Foreign.StablePtr

import Data.Eproj (EprojTagIndex, EprojTag(..), EprojTagProps(..))
import qualified Data.Eproj as Eproj

import Data.Emacs.Module.Args
import Data.Emacs.Module.SymbolName
import Data.Emacs.Module.SymbolName.TH
import Data.Regex
import Emacs.Module
import Emacs.Module.Assert
import Emacs.Module.Errors

initialise
  :: (WithCallStack, Throws EmacsThrow, Throws EmacsError, Throws EmacsInternalError)
  => EmacsM s ()
initialise = do
  bindFunction [esym|haskell-native--eproj-tag-index-p|] =<<
    makeFunction emacsEprojTagIndex emacsIsEprojTagIndexDoc
  bindFunction [esym|haskell-native--eproj-tag-index-empty|] =<<
    makeFunction emacsEmptyEprojTagIndex emacsEmptyEprojTagIndexDoc
  bindFunction [esym|haskell-native--eproj-tag-index-size|] =<<
    makeFunction emacsEprojTagIndexSize emacsEprojTagIndexSizeDoc
  bindFunction [esym|haskell-native--eproj-tag-index-add!|] =<<
    makeFunction emacsEprojTagIndexAdd emacsEprojTagIndexAddDoc
  bindFunction [esym|haskell-native--eproj-tag-index-lookup|] =<<
    makeFunction emacsEprojTagIndexLookup emacsEprojTagIndexLookupDoc
  bindFunction [esym|haskell-native--eproj-tag-index-entries-matching-re|] =<<
    makeFunction emacsEprojTagIndexEntriesMatchinRE emacsEprojTagIndexEntriesMatchinREDoc
  bindFunction [esym|haskell-native--eproj-tag-index-keys|] =<<
    makeFunction emacsEprojTagIndexKeys emacsEprojTagIndexKeysDoc
  bindFunction [esym|haskell-native--eproj-tag-index-entries|] =<<
    makeFunction emacsEprojTagIndexEntries emacsEprojTagIndexEntriesDoc
  bindFunction [esym|haskell-native--eproj-tag-index-drop-tags-from-file!|] =<<
    makeFunction emacsEprojTagIndexDropTagsFromFile emacsEprojTagIndexDropTagsFromFileDoc
  bindFunction [esym|haskell-native--eproj-tag-index-map-union!|] =<<
    makeFunction emacsEprojTagIndexMapUnion emacsEprojTagIndexUnionDoc

eprojTagIndexType
  :: forall m s. (WithCallStack, MonadEmacs m, Monad (m s))
  => m s (EmacsRef m s)
eprojTagIndexType = intern [esym|haskell-native--eproj-tag-index|]

isEprojTagIndex
  :: forall m s. (WithCallStack, MonadEmacs m, Monad (m s))
  => EmacsRef m s -> m s Bool
isEprojTagIndex x = do
  typ    <- typeOf x
  isCons <- eq typ =<< intern [esym|cons|]
  if isCons
    then do
      typ' <- car x
      eq typ' =<< eprojTagIndexType
    else pure False

unpackEmacsEprojTagIndex
  :: forall m s. (WithCallStack, MonadEmacs m, MonadIO (m s), MonadThrow (m s), Throws UserError)
  => EmacsRef m s -> m s EmacsEprojTagIndex
unpackEmacsEprojTagIndex x = do
  isTrie <- isEprojTagIndex x
  if isTrie
    then do
      (res :: EmacsEprojTagIndex) <- liftIO . deRefStablePtr =<< extractStablePtrFromUserPtr =<< cdr x
      pure res
    else do
      let str = "Prettyprinting temporarily omitted" :: String
      Checked.throw $ mkUserError "unpackEmacsEprojTagIndex" $ "Invalid native eproj tag index:" <+> pretty str

unpackEprojTagIndex
  :: forall m s. (WithCallStack, MonadEmacs m, MonadIO (m s), MonadThrow (m s), Throws UserError)
  => EmacsRef m s -> m s EprojTagIndex
unpackEprojTagIndex =
  liftIO . readIORef . unEmacsEprojTagIndex <=< unpackEmacsEprojTagIndex


newtype EmacsEprojTagIndex = EmacsEprojTagIndex
  { unEmacsEprojTagIndex :: IORef EprojTagIndex }

emacsEprojTagIndex
  :: forall m s. (WithCallStack, MonadEmacs m, Monad (m s))
  => EmacsFunction ('S 'Z) 'Z 'False s m
emacsEprojTagIndex (R x Stop) = do
  isIndex <- isEprojTagIndex x
  produceRef =<< if isIndex then intern [esym|t|] else nil

emacsIsEprojTagIndexDoc :: C8.ByteString
emacsIsEprojTagIndexDoc =
  "Check whether an Elisp value is a native eproj tag index."

emacsEmptyEprojTagIndex
  :: forall m s. (WithCallStack, MonadEmacs m, MonadIO (m s))
  => EmacsFunction 'Z 'Z 'False s m
emacsEmptyEprojTagIndex Stop = do
  typ <- eprojTagIndexType
  (radixTreePtr :: StablePtr EmacsEprojTagIndex) <- liftIO $ do
    index <- newIORef Eproj.empty
    newStablePtr $ EmacsEprojTagIndex index
  payload <- makeUserPtrFromStablePtr radixTreePtr
  produceRef =<< cons typ payload

emacsEmptyEprojTagIndexDoc :: C8.ByteString
emacsEmptyEprojTagIndexDoc =
  "Create an empty native eproj tag index."

emacsEprojTagIndexSize
  :: forall m s. (WithCallStack, MonadEmacs m, MonadIO (m s), MonadThrow (m s))
  => EmacsFunction ('S 'Z) 'Z 'False s m
emacsEprojTagIndexSize (R index Stop) = do
  index' <- unpackEprojTagIndex index
  produceRef =<< makeInt (Eproj.size index')

emacsEprojTagIndexSizeDoc :: C8.ByteString
emacsEprojTagIndexSizeDoc =
  "Get number of elements in a native eproj tag index."

emacsEprojTagIndexAdd
  :: forall m s. (WithCallStack, MonadEmacs m, MonadIO (m s), MonadThrow (m s), MonadCatch (m s))
  => EmacsFunction ('S ('S ('S ('S ('S 'Z))))) 'Z 'False s m
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
  produceRef =<< nil

emacsEprojTagIndexAddDoc :: C8.ByteString
emacsEprojTagIndexAddDoc =
  "Add an item to a native eproj tag index."

emacsEprojTagIndexLookup
  :: forall m s. (WithCallStack, MonadEmacs m, MonadIO (m s), MonadThrow (m s), MonadCatch (m s))
  => EmacsFunction ('S ('S 'Z)) ('S 'Z) 'False s m
emacsEprojTagIndexLookup (R key (R index (O def Stop))) = do
  index' <- unpackEprojTagIndex index
  key'   <- extractShortByteString key
  produceRef =<< case Eproj.lookup key' index' of
    Nothing -> maybe nil pure def
    Just x  -> makeList =<< traverse eprojTagToEmacs x

emacsEprojTagIndexLookupDoc :: C8.ByteString
emacsEprojTagIndexLookupDoc =
  "Lookup a tag within a native eproj tag index."

tagPropsToEmacs
  :: (MonadEmacs m, Monad (m s))
  => EprojTagProps
  -> m s (EmacsRef m s)
tagPropsToEmacs = unfoldEmacsListWith $ \case
  Nil         -> pure Nothing
  Cons x y rest -> do
    x' <- intern $ mkSymbolNameShortByteString x
    y' <- makeShortByteString y
    z  <- cons x' y'
    pure $ Just (z, rest)

eprojTagToEmacs
  :: (MonadEmacs m, Monad (m s))
  => EprojTag ShortByteString
  -> m s (EmacsRef m s)
eprojTagToEmacs EprojTag{etFile, etLine, etProps} = do
  etFile'  <- makeShortByteString etFile
  etLine'  <- makeInt etLine
  etProps' <- tagPropsToEmacs etProps
  funcallPrimitive [esym|make-eproj-tag|] [etFile', etLine', etProps']

emacsEprojTagIndexEntriesMatchinRE
  :: forall m s. (WithCallStack, MonadEmacs m, MonadIO (m s), MonadThrow (m s), MonadCatch (m s))
  => EmacsFunction ('S ('S ('S 'Z))) 'Z 'False s m
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

  produceRef =<< makeList =<< traverse eprojTagToEmacs (Eproj.keysMatchingRegexp regexp' index')

emacsEprojTagIndexEntriesMatchinREDoc :: C8.ByteString
emacsEprojTagIndexEntriesMatchinREDoc =
  "Get all tag entries whose key matches a regexp."

emacsEprojTagIndexKeys
  :: forall m s. (WithCallStack, MonadEmacs m, MonadIO (m s), MonadThrow (m s), MonadCatch (m s))
  => EmacsFunction ('S 'Z) 'Z 'False s m
emacsEprojTagIndexKeys (R index Stop) = do
  index' <- unpackEprojTagIndex index
  produceRef =<< makeList =<< traverse makeShortByteString (Eproj.keys index')

emacsEprojTagIndexKeysDoc :: C8.ByteString
emacsEprojTagIndexKeysDoc =
  "Get all keys of a native eproj tag index."

emacsEprojTagIndexEntries
  :: forall m s. (WithCallStack, MonadEmacs m, MonadIO (m s), MonadThrow (m s), MonadCatch (m s))
  => EmacsFunction ('S 'Z) 'Z 'False s m
emacsEprojTagIndexEntries (R index Stop) = do
  index' <- unpackEprojTagIndex index
  items  <- for (Eproj.toAscList index') $ \(key, vals) -> do
    key' <- makeShortByteString key
    cons key' =<< makeList =<< traverse eprojTagToEmacs vals
  produceRef =<< makeList items

emacsEprojTagIndexEntriesDoc :: C8.ByteString
emacsEprojTagIndexEntriesDoc =
  "Get all entries from a native eproj tag index in the form of (string . tags)."

emacsEprojTagIndexDropTagsFromFile
  :: forall m s. (WithCallStack, MonadEmacs m, MonadIO (m s), MonadThrow (m s), MonadCatch (m s))
  => EmacsFunction ('S ('S 'Z)) 'Z 'False s m
emacsEprojTagIndexDropTagsFromFile (R file (R index Stop)) = do
  file'  <- extractShortByteString file
  index' <- unpackEmacsEprojTagIndex index
  liftIO $ atomicModifyIORef (unEmacsEprojTagIndex index') $ \index'' ->
    (Eproj.dropTagsFromFile file' index'', ())
  produceRef =<< nil

emacsEprojTagIndexDropTagsFromFileDoc :: C8.ByteString
emacsEprojTagIndexDropTagsFromFileDoc =
  "Remove all tags that come from the specified file."

emacsEprojTagIndexMapUnion
  :: forall m s. (WithCallStack, MonadEmacs m, MonadIO (m s), MonadThrow (m s), MonadCatch (m s))
  => EmacsFunction ('S ('S 'Z)) 'Z 'False s m
emacsEprojTagIndexMapUnion (R index (R auxIndex Stop)) = do
  index'    <- unpackEmacsEprojTagIndex index
  auxIndex' <- unpackEprojTagIndex auxIndex
  liftIO $ atomicModifyIORef (unEmacsEprojTagIndex index') $ \index'' ->
    (Eproj.union index'' auxIndex', ())
  produceRef =<< nil

emacsEprojTagIndexUnionDoc :: C8.ByteString
emacsEprojTagIndexUnionDoc =
  "Destructively update first tag index with all elements of the second tag index."
