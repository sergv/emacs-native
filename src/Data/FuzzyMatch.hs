----------------------------------------------------------------------------
-- |
-- Module      :  Data.FuzzyMatch
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module Data.FuzzyMatch
  ( fuzzyMatch
  , computeHeatMap
  , Match(..)

  -- * Interface for testing
  , computeGroupsAndInitScores
  , HeatMapGroup(..)
  , StrIdx(..)
  ) where

import Control.Monad.State
import Control.Monad.ST

import Data.Bits
import Data.Char
import Data.Foldable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc (Pretty(..))
import Data.Traversable
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
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

isWord :: Char -> Bool
isWord = not . isWordSeparator

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
  , mPositions :: !(NonEmpty StrIdx)
  } deriving (Eq, Generic, Ord, Show)

data Submatch = Submatch
  { smScore           :: !Int
  , smPositions       :: !(NonEmpty StrIdx)
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
          case evalState (memoizeBy makeKey computeScore (NE.zip (0 :| [1..]) needle', StrIdx (-1))) mempty of
            []  -> noMatch
            [m] -> submatchToMatch m
            ms  -> submatchToMatch $ maximumBy (comparing smScore) ms
  where
    noMatch = Match
      { mScore     = 0
      , mPositions = StrIdx 0 :| []
      }
    occurs :: IntMap IntSet
    occurs = characterOccurrences haystack

    bigger :: StrIdx -> IntSet -> IntSet
    bigger x = snd . IS.split (unStrIdx x)

    makeKey :: (NonEmpty (Int, IntSet), StrIdx) -> Int
    makeKey ((!n, _) :| _, !k) =
      unsafeShiftL n (fromIntegral ((fromIntegral (finiteBitSize n) :: Word) `unsafeShiftR` 1)) .|. unStrIdx k

    computeScore
      :: Monad m
      => ((NonEmpty (Int, IntSet), StrIdx) -> m [Submatch])
      -> (NonEmpty (Int, IntSet), StrIdx)
      -> m [Submatch]
    computeScore recur (!indices, !cutoffIndex) =
      case indices of
        -- Last character
        (_, needleOccursInHaystack) :| [] ->
          pure $
            flip map (IS.toList remainingOccurrences) $ \idx ->
            Submatch
              { smScore           = heatmap U.! idx
              , smPositions       = StrIdx idx :| []
              , smContiguousCount = 0
              }
          where
            remainingOccurrences = bigger cutoffIndex needleOccursInHaystack
        (_, needleOccursInHaystack) :| idxs' : idxss ->
          fmap (getMaximum . concat) $ for (IS.toList remainingOccurrences) $ \idx -> do
            let idx' = StrIdx idx
            submatches <- recur (idxs' :| idxss, idx')
            pure $ getMaximum $ flip map submatches $ \submatch ->
              let score'          = smScore submatch + heatmap U.! unStrIdx idx'
                  contiguousBonus = 60 + 15 * min 3 (smContiguousCount submatch)
                  isContiguous    = NE.head (smPositions submatch) == succ idx'
                  score
                    | isContiguous
                    = score' + contiguousBonus
                    | otherwise
                    = score'
              in Submatch
                { smScore           = score
                , smPositions       = NE.cons idx' $ smPositions submatch
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

newtype StrIdx = StrIdx { unStrIdx :: Int }
  deriving (Eq, Ord, Show, Enum, Pretty)

data HeatMapGroup = HeatMapGroup
  { -- | At which index the group starts, inclusive. Usually points to
    -- separator that started the group, even for the first group where
    -- it's equal to -1. So, w.r.t. interesting group contents this index
    -- is exclusive.
    hmgStart       :: !StrIdx
    -- | At which index the group ends, inclusive.
  , hmgEnd         :: !StrIdx
  , hmgWordCount   :: !Int
    -- | Word indices
  , hmgWordIndices :: !IntSet
  , hmgIsBasePath  :: !Bool
  } deriving (Eq, Ord, Show, Generic)

splitWithSeps
  :: Char -- ^ Fake separator to add at the start
  -> IntSet
  -> Text
  -> [(Char, Text)]
splitWithSeps firstSep seps = go firstSep
  where
    go :: Char -> Text -> [(Char, Text)]
    go c str = (c, prefix) : rest
      where
        (prefix, suffix) = T.span ((`IS.notMember` seps) . ord) str
        rest = case T.uncons suffix of
          Nothing         -> []
          Just (c', str') -> go c' str'

computeHeatMap :: Text -> IntSet -> U.Vector Int
computeHeatMap str seps =
  computeHeatMapFromGroups str (computeGroupsAndInitScores str seps)

computeHeatMapFromGroups :: Text -> (Int, [HeatMapGroup]) -> U.Vector Int
computeHeatMapFromGroups fullStr (groupsCount, groups) = runST $ do
  scores <- UM.replicate len (initScore + initScoreAdjustment)
  update lastCharIdx lastCharBonus scores
  for_ groupScores' $ \(idx, val) -> update idx val scores
  for_ wordScores   $ \(idx, val) -> update idx val scores
  for_ penalties    $ \(idx, val) -> update idx val scores
  U.freeze scores
  where
    groupScores :: [(HeatMapGroup, Int)]
    groupScores =
      zipWith (\d g -> (g, groupBasicScore d g)) (-3 : iterate (+1) (-5)) groups

    groupScores' :: [(StrIdx, Int)]
    groupScores' = flip concatMap groupScores $ \(HeatMapGroup{hmgStart, hmgEnd}, score) ->
      map (, score) [succ hmgStart..min lastCharIdx (succ hmgEnd)]

    indexedWords :: [(StrIdx, StrIdx, Int)]
    indexedWords =
      fst $
      foldr
        (\HeatMapGroup{hmgWordIndices, hmgStart} (results, end) ->
          let newIndices =
                zipWith (\n (start, end') -> (start, end', n)) [0..]
                  $ fst
                  $ foldr
                      (\wordStart (xs, end') ->
                        let wordStart' = StrIdx wordStart in
                        ((wordStart', end') : xs, pred wordStart'))
                      ([], end)
                      (IS.toList hmgWordIndices)
          in (newIndices ++ results, hmgStart))
        ([], lastCharIdx)
        groups

    wordScores :: [(StrIdx, Int)]
    wordScores = flip concatMap indexedWords $ \(start, end, wordIdx) ->
      (start, 85) :
      zipWith (\wordChar pos -> (pos, wordIdx * (-3) - wordChar))
        [0..]
        [start..end]

    -- initScores' :: [(StrIdx, Int)]
    -- initScores' = case groups of
    --   []  -> initScores
    --   [_] -> initScores
    --   _   -> map (second (+ adjustment)) initScores
    --     where
    --       adjustment = (-2) * length groups

    penalties :: [(StrIdx, Int)]
    penalties
      = map (\(idx, _) -> (idx, -45))
      $ filter ((`IS.member` penalisedIfLeading) . ord . snd)
      $ zip [StrIdx 1..]
      $ T.unpack (T.init fullStr)

    penalisedIfLeading :: IntSet
    penalisedIfLeading = IS.fromList $ map ord ['.']

    initScoreAdjustment :: Int
    initScoreAdjustment = case groups of
      []  -> 0
      [_] -> 0
      _   -> (-2) * length groups

    update :: StrIdx -> Int -> UM.MVector s Int -> ST s ()
    update (StrIdx idx) val vec = do
      val' <- UM.unsafeRead vec idx
      UM.unsafeWrite vec idx (val' + val)

    initScore     = (-35)
    lastCharBonus = 1
    len           = T.length fullStr
    lastCharIdx   = StrIdx $ len - 1
    -- initScores :: [(StrIdx, Int)]
    -- initScores =
    --   (lastCharIdx, lastCharBonus) -- : map (, initScore) [StrIdx 0..pred lastCharIdx]

    groupBasicScore :: Int -> HeatMapGroup -> Int
    groupBasicScore nonBasePathDelta HeatMapGroup{hmgIsBasePath, hmgWordCount}
      | hmgIsBasePath = 35 + (if groupsCount > 2 then groupsCount - 2 else 0) - hmgWordCount
      | otherwise     = nonBasePathDelta

data GroupState = GroupState
  { gsBoundaryIndices :: !IntSet
  , gsWordCount       :: !Int
  }

computeGroupsAndInitScores :: Text -> IntSet -> (Int, [HeatMapGroup])
computeGroupsAndInitScores fullStr groupSeparators
  | T.null fullStr = (0, [])
  | otherwise
  = (groupsCount, )
  $ fst
  $ foldr (\x@HeatMapGroup{hmgIsBasePath} (xs, seenBasePath) ->
             (x { hmgIsBasePath = not seenBasePath && hmgIsBasePath } : xs, seenBasePath || hmgIsBasePath))
          ([], False)
  -- $ onHead (\x -> x { hmgStart = StrIdx 0 })
  $ map analyseGroup groups
  where
    analyseGroup :: (StrIdx, Char, StrIdx, Text) -> HeatMapGroup
    analyseGroup (start, prevChar, end, str) =
      HeatMapGroup
        { hmgStart       = start
        , hmgEnd         = end
        , hmgWordCount
        , hmgWordIndices = gsBoundaryIndices finalState
        , hmgIsBasePath  = hmgWordCount /= 0
        }
      where
        hmgWordCount = gsWordCount finalState
        finalState = L.foldl' step initState characters
        cs         = T.unpack str
        characters = zip3 [succ start..] (prevChar : cs) cs

        initState :: GroupState
        initState = GroupState
          { gsBoundaryIndices = mempty
          , gsWordCount       = 0
          }

        step :: GroupState -> (StrIdx, Char, Char) -> GroupState
        step GroupState{gsBoundaryIndices, gsWordCount} (idxCurrent, prev, current) =
          GroupState
            { gsBoundaryIndices =
              if haveBoundary prev current
              then IS.insert (unStrIdx idxCurrent) gsBoundaryIndices
              else gsBoundaryIndices
            , gsWordCount       =
              if not (isWord prev) && isWord current
              then 1 + gsWordCount
              else gsWordCount
            }

    groupsCount :: Int
    groups      :: [(StrIdx, Char, StrIdx, Text)]
    ((_, groupsCount), groups)
      = -- filter (\(_, _, len, _) -> len /= 0)
        mapAccumL
          (\(!idx, !len) (sep, str') ->
            let next = idx + T.length str' in
            ((next + 1, len + 1), (StrIdx idx, sep, StrIdx next, str')))
          ((-1) -- To account for fake separator at the beginning
          , 0
          )
      $ splitWithSeps ' ' groupSeparators fullStr

    -- Check whetehr @lastChar@ is the end of a word and
    -- @currentChar@ is the start of the next.
    haveBoundary :: Char -> Char -> Bool
    haveBoundary prevChar currentChar =
      res
      where
        res =
          not (isUpper prevChar) && isUpper currentChar ||
          not (isWord  prevChar) && isWord  currentChar
