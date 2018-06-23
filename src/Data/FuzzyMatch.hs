----------------------------------------------------------------------------
-- |
-- Module      :  Data.FuzzyMatch
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE OverloadedStrings #-}

module Data.FuzzyMatch
  ( fuzzyMatch
  , Match(..)
  ) where

import Control.Monad.State

import Data.Bits
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
import Data.Traversable
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
-- it occurs in.
--
-- Upper-case characters are counted twice as an upper-case and a
-- lower-case character. This is done in order to make lower-case
-- charaters match upper-case ones.
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
fuzzyMatch heatmap needle haystack =
  case T.unpack needle of
    [] -> noMatch
    n : ns ->
      case traverse (\c -> IM.lookup (ord c) occurs) (n :| ns) of
        Nothing      -> noMatch
        Just needle' ->
          case evalState (memoizeBy makeKey computeScore (NE.zip (0 :| [1..]) needle', (-1))) mempty of
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

    makeKey :: (NonEmpty (Int, IntSet), Int) -> Int
    makeKey ((!n, _) :| _, !k) =
      unsafeShiftL n (fromIntegral ((fromIntegral (finiteBitSize n) :: Word) `unsafeShiftR` 1)) .|. k

    computeScore
      :: Monad m
      => ((NonEmpty (Int, IntSet), Int) -> m [Submatch])
      -> (NonEmpty (Int, IntSet), Int)
      -> m [Submatch]
    computeScore recur (!indices, !cutoffIndex) =
      case indices of
        -- Last character
        (_, needleOccursInHaystack) :| [] ->
          pure $
            flip map (IS.toList remainingOccurrences) $ \idx ->
            Submatch
              { smScore           = heatmap U.! idx
              , smPositions       = idx :| []
              , smContiguousCount = 0
              }
          where
            remainingOccurrences = bigger cutoffIndex needleOccursInHaystack
        (_, needleOccursInHaystack) :| idxs' : idxss ->
          fmap (getMaximum . concat) $ for (IS.toList remainingOccurrences) $ \idx -> do
            submatches <- recur (idxs' :| idxss, idx)
            pure $ getMaximum $ flip map submatches $ \submatch ->
              let score'          = smScore submatch + heatmap U.! idx
                  contiguousBonus = 60 + 15 * min 3 (smContiguousCount submatch)
                  isContiguous    = NE.head (smPositions submatch) == idx + 1
                  score
                    | isContiguous
                    = score' + contiguousBonus
                    | otherwise
                    = score'
              in Submatch
                { smScore           = score
                , smPositions       = NE.cons idx $ smPositions submatch
                , smContiguousCount =
                  if isContiguous then smContiguousCount submatch + 1 else 0
                }
          where
            remainingOccurrences = bigger cutoffIndex needleOccursInHaystack

            getMaximum :: [Submatch] -> [Submatch]
            getMaximum [] = []
            getMaximum xs = (:[]) $ maximumBy (comparing smScore) xs

memoizeBy
  :: forall a b m. MonadState (IntMap b) m
  => (a -> Int)
  -> ((a -> m b) -> a -> m b)
  -> (a -> m b)
memoizeBy key f = g
  where
    g :: a -> m b
    g a = do
      store <- get
      let k = key a
      case IM.lookup k store of
        Just b  -> pure b
        Nothing -> do
          b <- f g a
          modify $ IM.insert k b
          pure b

