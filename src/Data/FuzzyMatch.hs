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
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

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
import Data.Traversable
import Data.Vector qualified as V
import Data.Vector.Ext qualified as VExt
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Growable qualified as VG
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Base qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import GHC.Generics (Generic)
import Prettyprinter (Pretty(..))

import Data.Text.Internal qualified as TI
import Data.Text.Unsafe qualified as TU

import Emacs.Module.Assert (WithCallStack)

-- data OccurrencesState = OccurrencesState
--   { osCharIndex :: !Int
--   , osIndices   :: !(IntMap IntSet)
--   }

isWordSeparator :: Char -> Bool
isWordSeparator c = not $ '0' <= c && c <= '9' || 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z'

isWord :: Char -> Bool
isWord = not . isWordSeparator



-- newtype PackedCharAndIdx = PackedCharAndIdx { unPackedCharAndIdx :: Int64 }
--
-- newtype instance U.MVector s PackedCharAndIdx = MV_PackedCharAndIdx (U.MVector s Int64)
-- newtype instance U.Vector    PackedCharAndIdx = V_PackedCharAndIdx  (U.Vector    Int64)
-- deriving instance GM.MVector U.MVector PackedCharAndIdx
-- deriving instance G.Vector   U.Vector  PackedCharAndIdx
-- instance U.Unbox PackedCharAndIdx
--
-- instance Eq PackedCharAndIdx where
--   (==) = (==) `on` ((`unsafeShiftR` 32) . unPackedCharAndIdx)
--
-- instance Ord PackedCharAndIdx where
--   compare = compare `on` ((`unsafeShiftR` 32) . unPackedCharAndIdx)

{-# INLINE textFoldM #-}
textFoldM :: forall m a. Monad m => (Char -> a -> m a) -> a -> Text -> m a
textFoldM f !seed (TI.Text arr off len) = go seed off
  where
    !end = off + len
    go :: a -> Int -> m a
    go !x !j
      | j >= end  = pure x
      | otherwise = do
        let TU.Iter c delta = TU.iterArray arr j
        x' <- f c x
        go x' (j + delta)

-- mkHaystack :: Text -> ST s (UM.MVector s Int64)
-- mkHaystack (TI.Text arr off len) = do
--   store <- VG.new (len + len `unsafeShiftR` 2)
--   go store 0 off
--   where
--     !end = off + len
--
--     go :: VG.GrowableVector (UM.MVector s Int64) -> Int -> Int -> ST s (UM.MVector s Int64)
--     go acc !i !j
--       | j >= end  = pure $ VG.finalise acc
--       | otherwise = do
--         let TU.Iter c delta = TU.iterArray arr j
--             !c'             = toLower c
--         acc' <-
--           if c == c'
--           then do
--             VG.push (combineCharIdx c i) acc
--           else do
--             VG.push (combineCharIdx c i) acc >>= VG.push (combineCharIdx c' i)
--         go acc' (i + 1) (j + delta)

mkHaystack :: forall s. Text -> ST s (UM.MVector s Int64)
mkHaystack str = do
  store <- VG.new (len + len `unsafeShiftR` 2)
  VG.finalise . snd <$> textFoldM go (0, store) str
  where
    !len = T.length str

    go
      :: Char
      -> (Int, VG.GrowableVector (UM.MVector s Int64))
      -> ST s (Int, VG.GrowableVector (UM.MVector s Int64))
    go !c (!i, !acc) = do
      let !c' = toLower c
      acc' <-
        if c == c'
        then do
          VG.push (combineCharIdx c i) acc
        else do
          VG.push (combineCharIdx c i) acc >>= VG.push (combineCharIdx c' i)
      pure (i + 1, acc')



combineCharIdx :: Char -> Int -> Int64
combineCharIdx c idx = (fi64 (ord c) `unsafeShiftL` 32) .|. fi64 idx

fi64 :: Integral a => a -> Int64
fi64 = fromIntegral

-- | For each character in the argument string compute the set of positions
-- it occurs in.
--
-- Upper-case characters are counted twice as an upper-case and a
-- lower-case character. This is done in order to make lower-case
-- charaters match upper-case ones.
characterOccurrences
  :: Text
  -> Text
  -> V.Vector (U.Vector StrIdx)
characterOccurrences needle haystack =
  V.map (findOccurs . fromIntegral . ord) $ V.fromList $ T.unpack needle
  where
    haystack' :: U.Vector Int64
    haystack' = runST $ do
      xs <- mkHaystack haystack
      VExt.qsort xs
      U.unsafeFreeze xs

    haystackChars :: U.Vector Int32
    haystackChars = U.map (fromIntegral . (`unsafeShiftR` 32)) haystack'

    haystackIdx :: U.Vector StrIdx
    haystackIdx = U.map (StrIdx . fromIntegral . (.&. 0x00000000FFFFFFFF)) haystack'

    haystackLen = U.length haystack'

    findOccurs :: Int32 -> U.Vector StrIdx
    findOccurs !c
      | isMember
      = U.unsafeSlice start (skipSameChars start - start) haystackIdx
      | otherwise
      = U.empty
      where
        (isMember, !start) =  VExt.binSearchMemberL c haystackChars

        skipSameChars :: Int -> Int
        skipSameChars !j
          | j == haystackLen
          = j
          | haystackChars `U.unsafeIndex` j == c
          = skipSameChars $ j + 1
          | otherwise
          = j

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
  -> Text          -- ^ Haystack
  -> Match
fuzzyMatch heatmap needle haystack
  | T.null needle     = noMatch
  | any U.null occurs = noMatch
  | otherwise         =
    case evalState (memoizeBy makeKey computeScore occurs (StrIdx (-1))) mempty of
      []  -> noMatch
      [m] -> submatchToMatch m
      ms  -> submatchToMatch $ maximumBy (comparing smScore) ms
  where
    noMatch = Match
      { mScore     = (-1)
      , mPositions = StrIdx (-1) :| []
      }

    occurs :: V.Vector (U.Vector StrIdx)
    occurs = characterOccurrences needle haystack

    bigger :: StrIdx -> U.Vector StrIdx -> U.Vector StrIdx
    bigger x xs
      | isMember  =
        let !i' = i + 1
        in U.unsafeSlice i' (U.length xs - i') xs
      | otherwise =
        U.unsafeSlice i (U.length xs - i) xs
      where
        (isMember, !i) = VExt.binSearchMemberIdx x xs

    makeKey :: V.Vector (U.Vector StrIdx) -> StrIdx -> Int64
    makeKey !occs !k =
      -- Todo: try Cantor and real hash tables
      fi64 j `unsafeShiftL` 32 .|. fi64 (unStrIdx k)
      where
        !j = V.length occs

    computeScore
      :: Monad m
      => (V.Vector (U.Vector StrIdx) -> StrIdx -> m [Submatch])
      -> V.Vector (U.Vector StrIdx)
      -> StrIdx
      -> m [Submatch]
    computeScore recur !needleOccursInHaystack !cutoffIndex =
      case V.length needleOccursInHaystack of
        -- Last character, already checke that vector is never empty
        1 ->
          pure $ flip map (U.toList remainingOccurrences) $ \(StrIdx idx) ->
            Submatch
              { smScore           = heatmap `indexPrimArray` fromIntegral idx
              , smPositions       = StrIdx idx :| []
              , smContiguousCount = 0
              }
          where
            remainingOccurrences = bigger cutoffIndex $ V.unsafeHead needleOccursInHaystack
        _ ->
          fmap (getMaximum . concat) $ for (U.toList remainingOccurrences) $ \(StrIdx idx) -> do
            let idx' = StrIdx idx
            submatches <- recur (V.unsafeTail needleOccursInHaystack) idx'
            pure $ getMaximum $ flip map submatches $ \submatch ->
              let score'          = smScore submatch + (heatmap `indexPrimArray` fromIntegral (unStrIdx idx'))
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
            remainingOccurrences = bigger cutoffIndex $ V.unsafeHead needleOccursInHaystack

            getMaximum :: [Submatch] -> [Submatch]
            getMaximum [] = []
            getMaximum xs = (:[]) $ maximumBy (comparing smScore) xs

memoizeBy
  :: forall a b c m. MonadState (IntMap c) m
  => (a -> b -> Int64)
  -> ((a -> b -> m c) -> a -> b -> m c)
  -> (a -> b -> m c)
memoizeBy key f = g
  where
    g :: a -> b -> m c
    g a b = do
      store <- get
      let !k = fromIntegral $ key a b
      case IM.lookup k store of
        Just c  -> pure c
        Nothing -> do
          c <- f g a b
          modify $! IM.insert k c
          pure c

newtype StrIdx = StrIdx { unStrIdx :: Int32 }
  deriving (Eq, Ord, Enum, Pretty)

instance Show StrIdx where
  show = show . unStrIdx

newtype instance U.MVector s StrIdx = MV_StrIdx (U.MVector s Int32)
newtype instance U.Vector    StrIdx = V_StrIdx  (U.Vector    Int32)
deriving instance GM.MVector U.MVector StrIdx
deriving instance G.Vector   U.Vector  StrIdx
instance U.Unbox StrIdx

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
  -> PrimArray Int
  -> Text
  -> [(Char, Text)]
splitWithSeps firstSep seps = go firstSep
  where
    go :: Char -> Text -> [(Char, Text)]
    go c str = (c, prefix) : rest
      where
        (prefix, suffix) = T.span (not . binSearchMember seps . ord) str
        rest = case T.uncons suffix of
          Nothing         -> []
          Just (c', str') -> go c' str'

computeHeatMap :: Text -> PrimArray Int -> PrimArray Int
computeHeatMap str =
  computeHeatMapFromGroups str . computeGroupsAndInitScores str

computeHeatMapFromGroups :: Text -> (Int, [HeatMapGroup]) -> PrimArray Int
computeHeatMapFromGroups fullStr (groupsCount, groups) = runPrimArray $ do
  scores <- newPrimArray len
  setPrimArray scores 0 len (initScore + initScoreAdjustment)
  -- scores <- UM.replicate len (initScore + initScoreAdjustment)
  update lastCharIdx lastCharBonus scores
  for_ groupScores' $ \(idx, val) -> update idx val scores
  for_ wordScores   $ \(idx, val) -> update idx val scores
  for_ penalties    $ \(idx, val) -> update idx val scores
  pure scores
  where
    groupScores :: [(HeatMapGroup, Int)]
    groupScores =
      zipWith (\d g -> (g, groupBasicScore d g)) (-3 : iterate (+ 1) (-5)) groups

    groupScores' :: [(StrIdx, Int)]
    groupScores' = flip concatMap groupScores $ \(HeatMapGroup{hmgStart, hmgEnd}, score) ->
      map (, score) [succ hmgStart..min lastCharIdx (succ hmgEnd)]

    indexedWords :: [(StrIdx, StrIdx, Int)]
    indexedWords =
      fst $
      foldr
        (\HeatMapGroup{hmgWordIndices, hmgStart} (results, end) ->
          let newIndices :: [(StrIdx, StrIdx, Int)]
              newIndices =
                zipWith (\n (start, end') -> (start, end', n)) [0..]
                  $ fst
                  $ foldr
                      (\wordStart (xs, end') ->
                        let wordStart' = StrIdx (fromIntegral wordStart) in
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
      val' <- readPrimArray vec (fromIntegral idx)
      writePrimArray vec (fromIntegral idx) (val' + val)

    initScore, lastCharBonus :: Int
    initScore     = (-35)
    lastCharBonus = 1
    len           = T.length fullStr
    lastCharIdx   = StrIdx $ fromIntegral $ len - 1
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

computeGroupsAndInitScores :: Text -> PrimArray Int -> (Int, [HeatMapGroup])
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
              then IS.insert (fromIntegral (unStrIdx idxCurrent)) gsBoundaryIndices
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
            ((next + 1, len + 1), (StrIdx (fromIntegral idx), sep, StrIdx (fromIntegral next), str')))
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
