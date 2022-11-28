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
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -Wno-overflowed-literals #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.FuzzyMatch
  ( fuzzyMatch
  , computeHeatMap
  , Match(..)
  , NeedleChars
  , prepareNeedle
  , ReusableState
  , mkReusableState

  -- * Interface for testing
  , computeGroupsAndInitScores
  , HeatMapGroup(..)
  , StrIdx(..)
  ) where

import Control.Monad.Primitive
import Control.Monad.ST.Strict
import Data.Bits
import Data.Char
import Data.Coerce
import Data.Foldable
import Data.Function
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
import Data.Primitive.PrimArray.Ext qualified as PExt
import Data.Primitive.PrimArray.Growable qualified as PG
import Data.Primitive.PrimArray.GrowableMut qualified as PGM
import Data.STRef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Ext qualified as T
import Data.Traversable
import Data.Vector qualified as V
import Data.Vector.Ext qualified as VExt
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Mutable qualified as VM
import Data.Vector.PredefinedSorts
import Data.Vector.Primitive qualified as P
import Data.Vector.Primitive.Mutable qualified as PM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Base qualified as U
import Data.Word
import GHC.Exts
import GHC.Generics (Generic)
import GHC.ST
import Numeric (showHex)
import Prettyprinter.Combinators
import Prettyprinter.Show

import Emacs.Module.Assert (WithCallStack)

{-# INLINE isWordSeparator #-}
isWordSeparator :: Char -> Bool
isWordSeparator c = not $ '0' <= c && c <= '9' || 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z'

{-# INLINE isWord #-}
isWord :: Char -> Bool
isWord = not . isWordSeparator

data ReusableState s = ReusableState
  { rsHaystackStore :: !(PGM.GrowablePrimArrayMut s Word64)
  , rsNeedleStore   :: !(VM.MVector s (U.Vector PackedIdx))
  }

mkReusableState :: PrimMonad m => Int -> NeedleChars -> m (ReusableState (PrimState m))
mkReusableState needleSize chars = do
  rsHaystackStore <- PGM.new $ needleCharsCountHint chars
  rsNeedleStore   <- VM.unsafeNew needleSize
  pure ReusableState{rsHaystackStore, rsNeedleStore}

newtype NeedleChars = NeedleChars { unNeedleChars :: P.Vector Char }

needleCharsCountHint :: NeedleChars -> Int
needleCharsCountHint = P.length . unNeedleChars

prepareNeedle :: Text -> NeedleChars
prepareNeedle str
  = NeedleChars
  $ VExt.uniq
  $ sortVectorUnsafeChar
  $ T.textToPrimVector
  $ T.map toUpper str <> str

needleMember :: Char -> NeedleChars -> Bool
needleMember c xs = {-# SCC "needleMember" #-} VExt.binSearchMember c . unNeedleChars $ xs


newtype PackedCharAndIdx = PackedCharAndIdx { _unPackedCharAndIdx :: Word64 }

newtype instance U.MVector s PackedCharAndIdx = MV_PackedCharAndIdx (U.MVector s Word64)
newtype instance U.Vector    PackedCharAndIdx = V_PackedCharAndIdx  (U.Vector    Word64)
deriving instance GM.MVector U.MVector PackedCharAndIdx
deriving instance G.Vector   U.Vector  PackedCharAndIdx
instance U.Unbox PackedCharAndIdx

instance Show PackedCharAndIdx where
  show (PackedCharAndIdx x) =
    show
      ( -- chr $ fromIntegral $
        showString "0x" . showHex (keepChar (PackedChar x) `unsafeShiftR` 32) $ []
      , keepIdx (PackedIdx x)
      )

instance Pretty PackedCharAndIdx where
  pretty = ppShow


newtype PackedChar = PackedChar { unPackedChar :: Word64 }

instance Show PackedChar where
  showsPrec _ =
    (showString "0x" .) . showHex . (`unsafeShiftR` 32) . keepChar

newtype instance U.MVector s PackedChar = MV_PackedChar (U.MVector s Word64)
newtype instance U.Vector    PackedChar = V_PackedChar  (U.Vector    Word64)
deriving instance GM.MVector U.MVector PackedChar
deriving instance G.Vector   U.Vector  PackedChar
instance U.Unbox PackedChar

mkPackedChar :: Char -> PackedChar
mkPackedChar = PackedChar . (`unsafeShiftL` 32) . w64 . ord

keepChar :: PackedChar -> Word64
keepChar =
  (.&. upper4Bytes) . unPackedChar
  -- (`unsafeShiftR` 32)

instance Eq PackedChar where
  (==) = (==) `on` keepChar

instance Ord PackedChar where
  compare = compare `on` keepChar


newtype PackedIdx = PackedIdx { unPackedIdx :: Word64 }

{-# INLINE upper4Bytes #-}
{-# INLINE lower4Bytes #-}
upper4Bytes, lower4Bytes :: Integral a => a
upper4Bytes = 0xFFFFFFFF00000000
lower4Bytes = 0x00000000FFFFFFFF

{-# INLINE keepIdx #-}
keepIdx :: PackedIdx -> StrIdx
keepIdx = StrIdx . fromIntegral . (.&. lower4Bytes) . unPackedIdx

{-# INLINE mkPackedIdx #-}
mkPackedIdx :: StrIdx -> PackedIdx
mkPackedIdx = PackedIdx . w64 . unStrIdx

{-# INLINE getStrIdx #-}
getStrIdx :: PackedIdx -> StrIdx
getStrIdx = keepIdx

newtype instance U.MVector s PackedIdx = MV_PackedIdx (U.MVector s Word64)
newtype instance U.Vector    PackedIdx = V_PackedIdx  (U.Vector    Word64)
deriving instance GM.MVector U.MVector PackedIdx
deriving instance G.Vector   U.Vector  PackedIdx
instance U.Unbox PackedIdx

instance Show PackedIdx where
  show = show . keepIdx

instance Pretty PackedIdx where
  pretty = ppShow

instance Eq PackedIdx where
  (==) = (==) `on` keepIdx

instance Ord PackedIdx where
  compare = compare `on` keepIdx

{-# NOINLINE mkHaystack #-}
mkHaystack :: forall s. ReusableState s -> NeedleChars -> Text -> ST s (PM.MVector s Word64)
mkHaystack (ReusableState store _) needleChars str = do
  -- store <- PGM.new (needleCharsCountHint needleChars)
  -- PGM.clear store
  PGM.with store $ \store' -> do

    let go
          :: forall ss. Int
          -> Char
          -> PG.GrowablePrimArrayU ss Word64
          -> State# ss
          -> (# State# ss, PG.GrowablePrimArrayU ss Word64 #)
        go !i !c arr s1
          | needleMember c needleChars =
            case PG.pushU (combineCharIdx c i) arr s1 of
              res@(# s2, arr' #) ->
                let !c' = toLower c in
                if c == c'
                then res
                else PG.pushU (combineCharIdx c' i) arr' s2
          | otherwise =
            (# s1, arr #)

    primitive $ \s ->
      case T.textFoldIdxM' go (PG.toUnboxed (PG.clear store')) str s of
        (# s2, x #) -> (# s2, PG.fromUnboxed x #)

  arr    <- PGM.finalise store
  arrLen <- PGM.size store
  -- arrLen <- getSizeofMutablePrimArray arr
  pure $ P.MVector 0 arrLen $ PExt.primToByteArr arr


{-# INLINE combineCharIdx #-}
combineCharIdx :: Char -> Int -> Word64
combineCharIdx c idx = (w64 (ord c) `unsafeShiftL` 32) .|. (lower4Bytes .&. w64 (fi32 idx))

{-# INLINE w64 #-}
w64 :: Integral a => a -> Word64
w64 = fromIntegral

-- {-# INLINE fi64 #-}
-- fi64 :: Integral a => a -> Int64
-- fi64 = fromIntegral

{-# INLINE fi32 #-}
fi32 :: Integral a => a -> Int32
fi32 = fromIntegral

-- | For each character in the argument string compute the set of positions
-- it occurs in.
--
-- Upper-case characters are counted twice as an upper-case and a
-- lower-case character. This is done in order to make lower-case
-- charaters match upper-case ones.

{-# NOINLINE characterOccurrences #-}
characterOccurrences
  :: ReusableState s
  -> Text
  -> NeedleChars
  -> Text
  -> ST s (V.MVector s (U.Vector PackedIdx), Bool)
characterOccurrences store@(ReusableState _ occursStore) !needle !needleChars !haystack = do
  -- occursStore <- VM.unsafeNew (T.length needle)
  haystackMut <- mkHaystack store needleChars haystack
  {-# SCC "haystack-sort" #-} qsortWord64 haystackMut
  (haystack' :: U.Vector Word64) <- U.V_Word64 <$> P.unsafeFreeze haystackMut

  let
    haystackChars :: U.Vector PackedChar
    !haystackChars = coerce haystack'

    haystackIdx :: U.Vector PackedIdx
    !haystackIdx = coerce haystack'

    -- haystackChars :: U.Vector Int32
    -- haystackChars = U.map (fromIntegral . (`unsafeShiftR` 32)) haystack'

    -- haystackIdx :: U.Vector StrIdx
    -- haystackIdx = U.map (StrIdx . fromIntegral . (.&. 0x00000000FFFFFFFF)) haystack'

    !haystackLen = U.length haystack'

    findOccurs :: Char -> U.Vector PackedIdx
    findOccurs !c
      | isMember
      = U.unsafeSlice start (skipSameChars start - start) haystackIdx
      | otherwise
      = U.empty
      where
        !c' = mkPackedChar c
        (isMember, !start) = VExt.binSearchMemberL c' haystackChars

        skipSameChars :: Int -> Int
        skipSameChars !j
          | j == haystackLen
          = j
          | keepChar (haystackChars `U.unsafeIndex` j) == unPackedChar c'
          = skipSameChars $ j + 1
          | otherwise
          = j

  -- T.textTraverseIdx_ (\i c -> VM.unsafeWrite occursStore i (findOccurs c)) needle
  -- T.textForIdx_ needle $ \i c ->
  --   VM.unsafeWrite occursStore i (findOccurs c)

  !anyEmpty <- T.textFoldIdxM
    (\ !i !c (!anyEmpty :: Bool) ->
        if anyEmpty
        then pure anyEmpty
        else do
          let !occs = findOccurs c
          VM.unsafeWrite occursStore i occs
          pure $ U.null occs)
    False
    needle
  -- V.unsafeFreeze occursStore
  pure (occursStore, anyEmpty)

data Match = Match
  { mScore     :: !Int32
  , mPositions :: !(NonEmpty StrIdx)
  } deriving (Eq, Generic, Ord, Show)

data Submatch = Submatch
  { smScore           :: !Int32
  , smPositions       :: !(NonEmpty StrIdx)
  , smContiguousCount :: !Int32
  } deriving (Generic, Show)

submatchToMatch :: Submatch -> Match
submatchToMatch Submatch{smScore, smPositions} = Match
  { mScore     = smScore
  , mPositions = smPositions
  }

fuzzyMatch
  :: forall s. WithCallStack
  => ReusableState s
  -> PrimArray Int32 -- ^ Heatmap mapping characters to scores
  -> Text            -- ^ Needle
  -> NeedleChars     -- ^ Sorted needle characters
  -> Text            -- ^ Haystack
  -> ST s Match
fuzzyMatch store heatmap needle needleChars haystack
  | T.null needle = pure noMatch
  | otherwise     = do
    (occurs :: V.MVector s (U.Vector PackedIdx), anyEmpty) <- characterOccurrences store needle needleChars haystack
    let
      bigger :: PackedIdx -> U.Vector PackedIdx -> U.Vector PackedIdx
      bigger x xs
        | isMember
        = let !i' = i + 1
          in U.unsafeSlice i' (U.length xs - i') xs
        | otherwise
        = U.unsafeSlice i (U.length xs - i) xs
        where
          (isMember, !i) = VExt.binSearchMemberIdx x xs

      computeScore
        :: PrimMonad m
        => (V.MVector (PrimState m) (U.Vector PackedIdx) -> PackedIdx -> m [Submatch])
        -> V.MVector (PrimState m) (U.Vector PackedIdx)
        -> PackedIdx
        -> m [Submatch]
      computeScore recur !needleOccursInHaystack !cutoffIndex = do
        (remainingOccurrences :: U.Vector PackedIdx) <- bigger cutoffIndex <$> VM.unsafeRead needleOccursInHaystack 0
        case VM.length needleOccursInHaystack of
          -- Last character, already checked that vector is never empty
          1 -> do
            pure $ flip map (U.toList remainingOccurrences) $ \pidx ->
              let StrIdx !idx = getStrIdx pidx
              in
              Submatch
                { smScore           = heatmap `indexPrimArray` fromIntegral idx
                , smPositions       = StrIdx idx :| []
                , smContiguousCount = 0
                }

          _ -> do
            fmap (getMaximum . concat) $ for (U.toList remainingOccurrences) $ \pidx -> do
              let !idx' = getStrIdx pidx
              submatches <- recur (VM.unsafeTail needleOccursInHaystack) pidx
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
              getMaximum :: [Submatch] -> [Submatch]
              getMaximum [] = []
              getMaximum xs = (:[]) $ maximumBy (comparing smScore) xs

    !result <-
      if anyEmpty -- Also catches occurs == V.empty
      then pure noMatch
      else do
        -- cache <- HT.newSized (T.length needle * 2)
        -- res   <- memoizeBy cache makeKey computeScore occurs (mkPackedIdx (StrIdx (-1)))
        res <- memoizeBy makeKey computeScore occurs (mkPackedIdx (StrIdx (-1)))
        pure $ case res of
          []  -> noMatch
          [m] -> submatchToMatch m
          ms  -> submatchToMatch $ maximumBy (comparing smScore) ms
    pure result
  where
    noMatch = Match
      { mScore     = (-1)
      , mPositions = StrIdx (-1) :| []
      }

    {-# NOINLINE makeKey #-}
    makeKey :: V.MVector s (U.Vector a) -> PackedIdx -> Int
    makeKey !occs !k =
      -- Todo: try Cantor and real hash tables
      j `unsafeShiftL` 32 .|. (lower4Bytes .&. fromIntegral (unStrIdx (keepIdx k)))
      where
        !j = VM.length occs

-- memoizeBy
--   :: forall a b c s.
--      HT.HashTable s Int64 c
--   -> (a -> b -> Int64)
--   -> ((a -> b -> ST s c) -> a -> b -> ST s c)
--   -> (a -> b -> ST s c)
-- memoizeBy cache mkKey f = g
--   where
--     g :: a -> b -> ST s c
--     g a b = do
--       let !k = mkKey a b
--       res <- HT.lookup cache k
--       case res of
--         Just c  -> pure c
--         Nothing -> do
--           c <- f g a b
--           HT.insert cache k c
--           pure c

memoizeBy
  :: forall a b c s.
     (a -> b -> Int)
  -> ((a -> b -> ST s c) -> a -> b -> ST s c)
  -> (a -> b -> ST s c)
memoizeBy mkKey f = \ !aa !bb -> do
  (cache :: STRef s (IntMap c)) <- newSTRef IM.empty
  let g :: a -> b -> ST s c
      g !a !b = do
        let !k = mkKey a b
        !res <- IM.lookup k <$> readSTRef cache
        case res of
          Just c  -> pure c
          Nothing -> do
            !c <- f g a b
            modifySTRef' cache $ IM.insert k c
            pure c
  g aa bb

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
  , hmgWordCount   :: !Int32
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
        (prefix, suffix) = T.span (not . PExt.binSearchMember seps . fromIntegral . ord) str
        rest = case T.uncons suffix of
          Nothing         -> []
          Just (c', str') -> go c' str'

computeHeatMap :: Text -> PrimArray Int32 -> PrimArray Int32
computeHeatMap str =
  computeHeatMapFromGroups str . computeGroupsAndInitScores str

computeHeatMapFromGroups :: Text -> (Int32, [HeatMapGroup]) -> PrimArray Int32
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
    groupScores :: [(HeatMapGroup, Int32)]
    groupScores =
      zipWith (\d g -> (g, groupBasicScore d g)) (-3 : iterate (+ 1) (-5)) groups

    groupScores' :: [(StrIdx, Int32)]
    groupScores' = flip concatMap groupScores $ \(HeatMapGroup{hmgStart, hmgEnd}, score) ->
      map (, score) [succ hmgStart..min lastCharIdx (succ hmgEnd)]

    indexedWords :: [(StrIdx, StrIdx, Int32)]
    indexedWords =
      fst $
      foldr
        (\HeatMapGroup{hmgWordIndices, hmgStart} (results, end) ->
          let newIndices :: [(StrIdx, StrIdx, Int32)]
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

    wordScores :: [(StrIdx, Int32)]
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

    penalties :: [(StrIdx, Int32)]
    penalties
      = map (\(idx, _) -> (idx, -45))
      $ filter (penalisedIfLeading . snd)
      $ zip [StrIdx 1..]
      $ T.unpack (T.init fullStr)

    penalisedIfLeading :: Char -> Bool
    penalisedIfLeading = (== '.')

    initScoreAdjustment :: Int32
    initScoreAdjustment = case groups of
      []  -> 0
      [_] -> 0
      _   -> (-2) * groupsCount

    update :: StrIdx -> Int32 -> MutablePrimArray s Int32 -> ST s ()
    update (StrIdx idx) val vec = do
      val' <- readPrimArray vec (fromIntegral idx)
      writePrimArray vec (fromIntegral idx) (val' + val)

    initScore, lastCharBonus :: Int32
    initScore     = (-35)
    lastCharBonus = 1
    len           = T.length fullStr
    lastCharIdx   = StrIdx $ fromIntegral $ len - 1
    -- initScores :: [(StrIdx, Int)]
    -- initScores =
    --   (lastCharIdx, lastCharBonus) -- : map (, initScore) [StrIdx 0..pred lastCharIdx]

    groupBasicScore :: Int32 -> HeatMapGroup -> Int32
    groupBasicScore nonBasePathDelta HeatMapGroup{hmgIsBasePath, hmgWordCount}
      | hmgIsBasePath = 35 + (if groupsCount > 2 then groupsCount - 2 else 0) - hmgWordCount
      | otherwise     = nonBasePathDelta

data GroupState = GroupState
  { gsBoundaryIndices :: !IntSet
  , gsWordCount       :: !Int32
  }

computeGroupsAndInitScores :: Text -> PrimArray Int32 -> (Int32, [HeatMapGroup])
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

    groupsCount :: Int32
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
