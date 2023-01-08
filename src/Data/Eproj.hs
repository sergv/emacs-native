----------------------------------------------------------------------------
-- |
-- Module      :  Data.Eproj
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Eproj
  ( EprojTagProps(..)
  , EprojTag(..)
  , EprojTagIndex
  , empty
  , size
  , lookup
  , insert
  , keysMatchingRegexp
  , keys
  , toAscList
  , dropTagsFromFile
  , union
  ) where

import Prelude hiding (lookup)

import Control.Arrow (second)
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.ByteString.Short (ShortByteString)
import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict qualified as M
import Data.Monoid (Sum(..))
import Data.Set (Set)
import Data.Set qualified as S
import GHC.Generics (Generic)

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as RadixTree
-- import Data.RadixTree (RadixTree)
-- import Data.RadixTree qualified as RadixTree

import Data.Regex (Regex, reMatchesShortByteString)

type RadixTree = Map ShortByteString

data EprojTagProps
  = Nil
  | Cons
      {-# UNPACK #-} !ShortByteString
      {-# UNPACK #-} !ShortByteString
      !EprojTagProps
  deriving (Eq, Ord, Show, Generic)

newtype FileId = FileId { unFileId :: Int }
  deriving (Eq, Ord, Show, Generic)

data EprojTag a = EprojTag
  { etFile  :: !a
  , etLine  :: {-# UNPACK #-} !Int
  , etProps :: !EprojTagProps
  } deriving (Eq, Ord, Show, Generic, Functor)

data EprojTagIndex = EprojTagIndex
  { eiItems :: !(RadixTree (NonEmpty (EprojTag FileId)))
  , eiFiles :: !(Bimap ShortByteString FileId)
  , eiSize  :: {-# UNPACK #-} !Int
  } deriving (Show, Generic)

empty :: EprojTagIndex
empty = EprojTagIndex
  { eiItems = RadixTree.empty
  , eiFiles = BM.empty
  , eiSize  = 0
  }

{-# INLINE size #-}
size :: EprojTagIndex -> Int
size = eiSize

{-# INLINE lookup #-}
lookup
  :: ShortByteString
  -> EprojTagIndex
  -> Maybe (NonEmpty (EprojTag ShortByteString))
lookup symbol EprojTagIndex{eiItems, eiFiles} =
  fmap (fmap (eiFiles BM.!>)) <$> RadixTree.lookup symbol eiItems

insert :: ShortByteString -> EprojTag ShortByteString -> EprojTagIndex -> EprojTagIndex
insert symbol tag EprojTagIndex{eiItems, eiFiles, eiSize} =
  EprojTagIndex
    { eiItems = RadixTree.insertWith (<>) symbol (tag' :| []) eiItems
    , eiFiles = eiFiles'
    , eiSize  = eiSize + 1
    }
  where
    tag' = fileId <$ tag
    (fileId, eiFiles') =
      case BM.lookup (etFile tag) eiFiles of
        Nothing -> (nextId, BM.insert (etFile tag) nextId eiFiles)
          where
            nextId = FileId $ BM.size eiFiles
        Just x  -> (x, eiFiles)


-- | Query tag index by matching keys against a regex.
keysMatchingRegexp :: Regex -> EprojTagIndex -> [EprojTag ShortByteString]
keysMatchingRegexp re EprojTagIndex{eiItems, eiFiles}
  = map (fmap (eiFiles BM.!>))
  $ concatMap (toList . snd)
  $ filter (reMatchesShortByteString re . fst)
  $ RadixTree.toAscList eiItems

keys :: EprojTagIndex -> [ShortByteString]
keys = RadixTree.keys . eiItems

toAscList :: EprojTagIndex -> [(ShortByteString, NonEmpty (EprojTag ShortByteString))]
toAscList EprojTagIndex{eiItems, eiFiles} =
  map (second (fmap (fmap (eiFiles BM.!>)))) $ RadixTree.toAscList eiItems

dropTagsFromFile :: ShortByteString -> EprojTagIndex -> EprojTagIndex
dropTagsFromFile filename index@EprojTagIndex{eiItems, eiFiles} =
  case BM.lookup filename eiFiles of
    Nothing  -> index
    Just idx -> index
      { eiItems = eiItems'
      , eiSize  = getSum $ foldMap (Sum . length) eiItems'
      }
      where
        eiItems' = RadixTree.mapMaybe g eiItems
        g :: NonEmpty (EprojTag FileId) -> Maybe (NonEmpty (EprojTag FileId))
        g tags = case filter (\EprojTag{etFile} -> etFile /= idx) $ toList tags of
          []     -> Nothing
          y : ys -> Just (y :| ys)

union :: EprojTagIndex -> EprojTagIndex -> EprojTagIndex
union (EprojTagIndex items ids _) (EprojTagIndex items' ids' _) =
  EprojTagIndex
    { eiItems
    , eiFiles
    , eiSize  = getSum $ foldMap (Sum . length) eiItems
    }
  where
    eiItems = RadixTree.unionWith (<>) items $ fmap (fmap ((eiFiles BM.!) . (ids' BM.!>))) <$> items'

    eiFiles :: Bimap ShortByteString FileId
    eiFiles = S.foldl' (\acc key -> BM.insert key (FileId (BM.size acc)) acc) ids missingKeys

    missingKeys :: Set ShortByteString
    missingKeys = M.keysSet (BM.toMap ids) `S.difference` M.keysSet (BM.toMap ids')
