----------------------------------------------------------------------------
-- |
-- Module      :  Data.FuzzyMatch
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# LANGUAGE OverloadedStrings #-}

module Data.FuzzyMatch
  ( fuzzyMatch
  , Match(..)
  ) where

import Data.Char
import Data.Foldable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import GHC.Generics (Generic)

import Emacs.Module.Assert (WithCallStack)

data OccurrencesState = OccurrencesState
  { osCharIndex :: !Int
  , osIndices   :: !(IntMap IntSet)
  }

isWordSeparator :: Char -> Bool
isWordSeparator = \case
  ' '  -> True
  '*'  -> True
  '+'  -> True
  '-'  -> True
  '_'  -> True
  ':'  -> True
  ';'  -> True
  '.'  -> True
  '/'  -> True
  '\\' -> True
  _    -> False

isCapital :: Char -> Bool
isCapital c = not (isWordSeparator c) && isUpper c

-- | For each character in the argument string compute the set of positions
-- it occurs in. Upper-case characters are counted twice as an upper-case
-- and a lower-case character.
characterOccurrences
  :: Text
  -> IntMap IntSet
characterOccurrences = osIndices . T.foldl' recordIndex initState
  where
    initState = OccurrencesState
      { osCharIndex = 0
      , osIndices   = mempty
      }

    recordIndex :: OccurrencesState -> Char -> OccurrencesState
    recordIndex OccurrencesState{osCharIndex, osIndices} c =
      OccurrencesState
        { osCharIndex = osCharIndex + 1
        , osIndices   =
            if isCapital c
            then IM.insertWith (<>) (ord c)           pos $
                 IM.insertWith (<>) (ord (toLower c)) pos osIndices
            else IM.insertWith (<>) (ord c)           pos osIndices
        }
      where
        pos = IS.singleton osCharIndex

data Match = Match
  { mScore     :: !Int
  , mPositions :: !(NonEmpty Int)
  } deriving (Eq, Generic, Ord, Show)

data Submatch = Submatch
  { smScore           :: !Int
  , smPositions       :: !(NonEmpty Int)
  , smContiguousCount :: !Int
  } deriving (Generic, Show)

submatchToMatch :: Submatch -> Match
submatchToMatch Submatch{smScore, smPositions} = Match
  { mScore     = smScore
  , mPositions = smPositions
  }

fuzzyMatch
  :: WithCallStack
  => U.Vector Int -- ^ Heatmap mapping characters to scores
  -> Text         -- ^ Needle
  -> Text         -- ^ Haystack
  -> Match
fuzzyMatch heatmap needle haystack
  | T.null needle
  = noMatch
  | otherwise
  = case computeScore needle' (-1) of
      []  -> noMatch
      [m] -> submatchToMatch m
      ms  -> error $ "More than one optimal match found:" ++ show ms
  where
    noMatch = Match
      { mScore     = 0
      , mPositions = 0 :| []
      }
    occurs :: IntMap IntSet
    occurs = characterOccurrences haystack

    bigger :: Int -> IntSet -> IntSet
    bigger x = snd . IS.split x

    needle' :: [Char]
    needle' = T.unpack needle

    computeScore :: [Char] -> Int -> [Submatch]
    computeScore cs !cutoffIndex =
      case cs of
        []  -> []
        [lastChar] -> case IM.lookup (ord lastChar) occurs of
          Nothing   -> []
          Just idxs ->
            flip map (IS.toList remainingIndices) $ \idx ->
            Submatch
              { smScore           = heatmap U.! idx
              , smPositions       = idx :| []
              , smContiguousCount = 0
              }
            where
              remainingIndices = bigger cutoffIndex idxs
        c : cs' ->
          case IM.lookup (ord c) occurs of
            Nothing   -> []
            Just idxs -> getMaximum
              [ submatch'
              | idx      <- IS.toList remainingIndices
              , submatch <- computeScore cs' idx
              , let score'          = smScore submatch + heatmap U.! idx
                    contiguousBonus = 60 + 15 * min 3 (smContiguousCount submatch)
                    isContiguous    = NE.head (smPositions submatch) == idx + 1
                    score
                      | isContiguous
                      = score' + contiguousBonus
                      | otherwise
                      = score'
                    submatch' = Submatch
                      { smScore           = score
                      , smPositions       = NE.cons idx $ smPositions submatch
                      , smContiguousCount =
                        if isContiguous then smContiguousCount submatch + 1 else 0
                      }
              ]
              where
                remainingIndices = bigger cutoffIndex idxs

                getMaximum :: [Submatch] -> [Submatch]
                getMaximum [] = []
                getMaximum xs = (:[]) $ maximumBy (comparing smScore) xs
