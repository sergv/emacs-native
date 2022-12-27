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
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module Data.FuzzyMatchBaseline
  ( fuzzyMatch
  , computeHeatmap
  , Match(..)
  , NeedleChars
  , prepareNeedle

  -- * Interface for testing
  , computeGroupsAndInitScores
  , HeatmapGroup(..)
  , StrIdx(..)
  ) where

import Control.Monad.State
import Control.Monad.ST

import Data.Bits
import Data.Char
import Data.Foldable
import Data.Int
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Ord
import Data.Primitive.PrimArray
import Data.Primitive.PrimArray.Ext
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Ext qualified as T
import Data.Traversable
import Data.Vector.Ext qualified as VExt
import Data.Vector.Primitive qualified as P
import GHC.Generics (Generic)
import Prettyprinter (Pretty(..))

import Emacs.Module.Assert (WithCallStack)

data OccurrencesState = OccurrencesState
  { osCharIndex :: !Int
  , osIndices   :: !(IntMap IntSet)
  }

isWordSeparator :: Char -> Bool
isWordSeparator c = not $ '0' <= c && c <= '9' || 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z'

isWord :: Char -> Bool
isWord = not . isWordSeparator

newtype NeedleChars = NeedleChars { unNeedleChars :: P.Vector Char }

_needleCharsCountHint :: NeedleChars -> Int
_needleCharsCountHint = P.length . unNeedleChars

prepareNeedle :: Text -> NeedleChars
prepareNeedle str
  = NeedleChars
  $ VExt.uniq
  $ VExt.sortVectorUnsafe
  $ T.textToPrimVector
  $ T.map toUpper str <> str

needleMember :: Char -> NeedleChars -> Bool
needleMember c = VExt.binSearchMember c . unNeedleChars


-- | For each character in the argument string compute the set of positions
-- it occurs in.
--
-- Upper-case characters are counted twice as an upper-case and a
-- lower-case character. This is done in order to make lower-case
-- charaters match upper-case ones.
{-# NOINLINE characterOccurrences #-}
characterOccurrences
  :: NeedleChars
  -> Text
  -> IntMap IntSet
characterOccurrences needleChars = osIndices . T.foldl' recordIndex initState
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
          if needleMember c needleChars
          then
            let !c'  = ord c
                !cl' = ord $ toLower c
            in if c' == cl'
            then IM.insertWith (<>) c'  pos osIndices
            else IM.insertWith (<>) c'  pos $
                 IM.insertWith (<>) cl' pos osIndices
          else osIndices
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
  => PrimArray Int -- ^ Heatmap mapping characters to scores
  -> Text          -- ^ Needle
  -> NeedleChars
  -> Text          -- ^ Haystack
  -> Match
fuzzyMatch heatmap needle needleChars haystack =
  case T.unpack needle of
    []     -> noMatch
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
      { mScore     = (-1)
      , mPositions = StrIdx (-1) :| []
      }
    occurs :: IntMap IntSet
    occurs = characterOccurrences needleChars haystack

    bigger :: StrIdx -> IntSet -> IntSet
    bigger x = snd . IS.split (unStrIdx x)

    makeKey :: (NonEmpty (Int, IntSet), StrIdx) -> Int
    makeKey ((!n, _) :| _, !k) =
      -- n `unsafeShiftL` 32 .|. unStrIdx k
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
              { smScore           = heatmap `indexPrimArray` idx
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
              let score'          = smScore submatch + (heatmap `indexPrimArray` unStrIdx idx')
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

data HeatmapGroup = HeatmapGroup
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
  -> PrimArray Int32
  -> Text
  -> [(Char, Text)]
splitWithSeps firstSep seps = go firstSep
  where
    go :: Char -> Text -> [(Char, Text)]
    go c str = (c, prefix) : rest
      where
        (prefix, suffix) = T.span (not . binSearchMember seps . fromIntegral . ord) str
        rest = case T.uncons suffix of
          Nothing         -> []
          Just (c', str') -> go c' str'

computeHeatmap :: Text -> PrimArray Int32 -> PrimArray Int
computeHeatmap str =
  computeHeatmapFromGroups str . computeGroupsAndInitScores str

computeHeatmapFromGroups :: Text -> (Int, [HeatmapGroup]) -> PrimArray Int
computeHeatmapFromGroups fullStr (groupsCount, groups) = runPrimArray $ do
  scores <- newPrimArray len
  setPrimArray scores 0 len (initScore + initScoreAdjustment)
  update lastCharIdx lastCharBonus scores
  for_ groupScores' $ \(idx, val) -> update idx val scores
  for_ wordScores   $ \(idx, val) -> update idx val scores
  for_ penalties    $ \(idx, val) -> update idx val scores
  pure scores
  where
    groupScores :: [(HeatmapGroup, Int)]
    groupScores =
      zipWith (\d g -> (g, groupBasicScore d g)) (-3 : iterate (+ 1) (-5)) groups

    groupScores' :: [(StrIdx, Int)]
    groupScores' = flip concatMap groupScores $ \(HeatmapGroup{hmgStart, hmgEnd}, score) ->
      map (, score) [succ hmgStart..min lastCharIdx (succ hmgEnd)]

    indexedWords :: [(StrIdx, StrIdx, Int)]
    indexedWords =
      fst $
      foldr
        (\HeatmapGroup{hmgWordIndices, hmgStart} (results, end) ->
          let newIndices :: [(StrIdx, StrIdx, Int)]
              newIndices =
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
      $ filter (penalisedIfLeading . snd)
      $ zip [StrIdx 1..]
      $ T.unpack (T.init fullStr)

    penalisedIfLeading :: Char -> Bool
    penalisedIfLeading = (== '.')

    initScoreAdjustment :: Int
    initScoreAdjustment = case groups of
      []  -> 0
      [_] -> 0
      _   -> (-2) * groupsCount

    update :: StrIdx -> Int -> MutablePrimArray s Int -> ST s ()
    update (StrIdx idx) val vec = do
      val' <- readPrimArray vec idx
      writePrimArray vec idx (val' + val)

    initScore, lastCharBonus :: Int
    initScore     = (-35)
    lastCharBonus = 1
    len           = T.length fullStr
    lastCharIdx   = StrIdx $ len - 1
    -- initScores :: [(StrIdx, Int)]
    -- initScores =
    --   (lastCharIdx, lastCharBonus) -- : map (, initScore) [StrIdx 0..pred lastCharIdx]

    groupBasicScore :: Int -> HeatmapGroup -> Int
    groupBasicScore nonBasePathDelta HeatmapGroup{hmgIsBasePath, hmgWordCount}
      | hmgIsBasePath = 35 + (if groupsCount > 2 then groupsCount - 2 else 0) - hmgWordCount
      | otherwise     = nonBasePathDelta

data GroupState = GroupState
  { gsBoundaryIndices :: !IntSet
  , gsWordCount       :: !Int
  }

computeGroupsAndInitScores :: Text -> PrimArray Int32 -> (Int, [HeatmapGroup])
computeGroupsAndInitScores fullStr groupSeparators
  | T.null fullStr = (0, [])
  | otherwise
  = (groupsCount, )
  $ fst
  $ foldr (\x@HeatmapGroup{hmgIsBasePath} (xs, seenBasePath) ->
             (x { hmgIsBasePath = not seenBasePath && hmgIsBasePath } : xs, seenBasePath || hmgIsBasePath))
          ([], False)
  -- $ onHead (\x -> x { hmgStart = StrIdx 0 })
  $ map analyseGroup groups
  where
    analyseGroup :: (StrIdx, Char, StrIdx, Text) -> HeatmapGroup
    analyseGroup (start, prevChar, end, str) =
      HeatmapGroup
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
