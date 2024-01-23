----------------------------------------------------------------------------
-- |
-- Module      :  Data.FuzzyMatch
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE CPP               #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnboxedTuples     #-}
{-# LANGUAGE UnliftedFFITypes  #-}
{-# LANGUAGE UnliftedNewtypes  #-}

module Data.FuzzyMatch
  ( fuzzyMatch'
  , fuzzyMatch
  , Match(..)
  , NeedleChars
  , prepareNeedle
  , splitNeedle
  , ReusableState
  , mkReusableState

  -- * Interface for testing
  , computeHeatmap
  , Heatmap(..)
  , Heat(..)
  , StrCharIdx(..)
  ) where

import Control.Monad
import Control.Monad.ST.Strict
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Foldable1 as Foldable1
import Data.Int
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.MinMaxIdx
import Data.Monoid (Dual(..))
import Data.Ord
import Data.Primitive.ByteArray
import Data.Primitive.PrimArray
import Data.Primitive.PrimArray.Ext qualified as PExt
import Data.Primitive.Types
import Data.STRef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Array qualified as TA
import Data.Text.Ext qualified as T
import Data.Text.Internal qualified as TI
import Data.Text.Internal.Private qualified as T (spanAscii_)
import Data.Text.Unsafe qualified as T
import Data.Traversable
import Data.Vector qualified as V
import Data.Vector.Ext qualified as VExt
import Data.Vector.Mutable qualified as VM
import Data.Vector.PredefinedSorts
import Data.Vector.Primitive qualified as P
import Data.Vector.Primitive.Mutable qualified as PM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Base qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Data.Word
import GHC.Int (Int32(I32#))
import GHC.Magic (inline)
import GHC.ST
import GHC.Types
import GHC.Word (Word64(W64#))
import Prettyprinter.Generics

import Data.Packed
import Data.StrIdx
import Emacs.Module.Assert (WithCallStack)

#if __GLASGOW_HASKELL__ < 904

import GHC.Prim hiding (Word64#)

type Word64# = Word#

wordToWord64# :: Word# -> Word64#
wordToWord64# x = x

#else

import GHC.Prim

#endif

{-# INLINE isWord #-}
isWord :: Int# -> Bool#
isWord x = case chr# x of
  ' '#  -> False#
  '\t'# -> False#
  '\r'# -> False#
  '\n'# -> False#
  '*'#  -> False#
  '+'#  -> False#
  '-'#  -> False#
  '_'#  -> False#
  ':'#  -> False#
  ';'#  -> False#
  '.'#  -> False#
  ','#  -> False#
  '/'#  -> False#
  '\\'# -> False#
  _     -> True#

data ReusableState s = ReusableState
  { rsHaystackStore :: !(STRef s (UM.MVector s CharAndIdxs))
  , rsHeatmapStore  :: !(STRef s (MutablePrimArray s Heat))
  , rsNeedleStore   :: !(VM.MVector s (U.Vector PackedStrCharIdxAndStrByteIdx))
  }

-- Needle size here must cover all possible needle sizes that are going to be passed.
mkReusableState :: Int -> ST s (ReusableState s)
mkReusableState !needleSize = do
  rsHaystackStore <- newSTRef =<< UM.unsafeNew needleSize
  rsHeatmapStore  <- newSTRef =<< newPrimArray needleSize
  rsNeedleStore   <- VM.unsafeNew needleSize
  pure ReusableState{rsHaystackStore, rsHeatmapStore, rsNeedleStore}

newtype Bytes8 = Bytes8 Word64
  deriving (Generic, Pretty)
data Bytes16 = Bytes16 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  deriving (Generic)
data Bytes24 = Bytes24 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  deriving (Generic)
data Bytes32 = Bytes32 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  deriving (Generic)
newtype CharsVector = CharsVector { unCharsVector :: P.Vector Char }
  deriving (Generic)

instance Pretty Bytes16 where
  pretty = ppGeneric
instance Pretty Bytes24 where
  pretty = ppGeneric
instance Pretty Bytes32 where
  pretty = ppGeneric
instance Pretty CharsVector where
  pretty = pretty . P.toList . unCharsVector

class CharMember a where
  charMember :: Char# -> a -> Bool

instance CharMember Bytes8 where
  charMember charCode (Bytes8 w1) = isTrue# (charCode' <# 128#) && wordMember w1 (fanout charCode')
    where
      !charCode' = ord# charCode

instance CharMember Bytes16 where
  charMember charCode (Bytes16 w1 w2) = isTrue# (charCode' <# 128#) && (wordMember w1 bytes || wordMember w2 bytes)
    where
      !bytes = fanout charCode'
      !charCode' = ord# charCode

instance CharMember Bytes24 where
  charMember charCode (Bytes24 w1 w2 w3) = isTrue# (charCode' <# 128#) && (wordMember w1 bytes || wordMember w2 bytes || wordMember w3 bytes)
    where
      !bytes = fanout charCode'
      !charCode' = ord# charCode

instance CharMember Bytes32 where
  charMember charCode (Bytes32 w1 w2 w3 w4) = isTrue# (charCode' <# 128#) && (wordMember w1 bytes || wordMember w2 bytes || wordMember w3 bytes ||  wordMember w4 bytes)
    where
      !bytes = fanout charCode'
      !charCode' = ord# charCode

instance CharMember CharsVector where
  charMember charCode (CharsVector arr) = VExt.binSearchMember (C# charCode) arr

hasZeroByte :: Word64 -> Bool
hasZeroByte !x = 0 /= (((x - 0x0101010101010101) .&. complement x .&. 0x8080808080808080))

-- hasZeroByte :: Word64# -> Bool
-- hasZeroByte x =
--   isTrue# (wordToWord64# 0## `ltWord64#` (((x `subWord64#` wordToWord64# 0x0101010101010101##) `and64#` not64# x `and64#` wordToWord64# 0x8080808080808080##)))

wordMember :: Word64 -> ByteFanout -> Bool
wordMember !big !(ByteFanout small) = hasZeroByte (small `xor` big)

newtype ByteFanout = ByteFanout Word64

fanout :: Int# -> ByteFanout
fanout x = ByteFanout ((W64# (wordToWord64# (int2Word# x))) * 0x0101010101010101)

data NeedleChars
  = NeedleChars8 {-# UNPACK #-} !Bytes8
  | NeedleChars16 {-# UNPACK #-} !Bytes16
  | NeedleChars24 {-# UNPACK #-} !Bytes24
  | NeedleChars32 {-# UNPACK #-} !Bytes32
  | NeedleCharsLong {-# UNPACK #-} !CharsVector
  deriving (Generic)

instance Pretty NeedleChars where
  pretty = ppGeneric

prepareNeedle :: Text -> NeedleChars
prepareNeedle str
  | T.all (\c -> ord c < 128) str' && len < 33
  =
    if len < 17
    then
      if len < 9
      then NeedleChars8 $ Bytes8 (readBytes arr offset len)
      else NeedleChars16 $ Bytes16 (readWord64 arr offset64) (readBytes arr (offset + 8) (len - 8))
    else
      if len < 25
      then NeedleChars24 $ Bytes24 (readWord64 arr offset64) (readWord64 arr (offset64 + 1)) (readBytes arr (offset + 16) (len - 16))
      else NeedleChars32 $ Bytes32 (readWord64 arr offset64) (readWord64 arr (offset64 + 1)) (readWord64 arr (offset64 + 2)) (readBytes arr (offset + 24) (len - 24))
  | otherwise
  = NeedleCharsLong
  $ CharsVector
  $ VExt.uniq
  $ sortVectorUnsafeChar
  $ T.textToPrimVector
  $ str'
  where
    str' = str <> T.toUpper str
    TI.Text arr offset len = str'
    offset64 = offset `unsafeShiftR` 3

readWord64 :: TA.Array -> Int -> Word64
readWord64 (TA.ByteArray barr) (I# start) =
  case indexWord64Array# barr start of
    x -> W64# x

readBytes :: TA.Array -> Int -> Int -> Word64
readBytes xs start count = go 0 start
  where
    !end = start + count
    go :: Word64 -> Int -> Word64
    go !acc !i
      | i == end
      = acc
      | otherwise
      = go (acc `unsafeShiftL` 8 .|. fromIntegral (TA.unsafeIndex xs i)) (i + 1)

instance CharMember NeedleChars where
  charMember charCode = \case
    NeedleChars8    x  -> charMember charCode x
    NeedleChars16   x  -> charMember charCode x
    NeedleChars24   x  -> charMember charCode x
    NeedleChars32   x  -> charMember charCode x
    NeedleCharsLong xs -> charMember charCode xs

{-# NOINLINE mkHaystack #-}
mkHaystack :: forall s. ReusableState s -> NeedleChars -> Text -> ST s (UM.MVector s CharAndIdxs)
mkHaystack ReusableState{rsHaystackStore} !needleChars !haystack@(TI.Text _ _ haystackBytes) = do
  -- store <- PGM.new (needleCharsCountHint needleChars)
  arr <- readSTRef rsHaystackStore

  -- Upper bound is twice the haystack size because each uppercase
  -- needle character may be counted twice - once as uppercase second
  -- as lowercase.
  let toReserve = 2 * haystackBytes * I# (sizeOf# (undefined :: Word64))

  let !currSize = UM.length arr

  arr' <-
    if toReserve <= currSize
    then pure arr
    else do
      arr' <- UM.unsafeNew toReserve
      writeSTRef rsHaystackStore arr'
      pure arr'

  let goAscii
        :: (Char# -> Bool)
        -> StrCharIdx Int
        -> StrByteIdx Int
        -> Char
        -> Int
        -> ST s Int
      goAscii memberPred !cidx !bidx !charCode@(C# charCode#) j
        | memberPred charCode# = do
          let !packedIdxs = mkPackedStrCharIdxAndStrByteIdx (fromIntegral <$> cidx) (fromIntegral <$> bidx)
          UM.unsafeWrite arr' j $ CharAndIdxs charCode packedIdxs
          if toBool (isUpperASCII charCode')
          then do
            UM.unsafeWrite arr' (j + 1) (CharAndIdxs (C# (chr# (toLowerASCII charCode'))) packedIdxs)
            pure $! j + 2
          else
            pure $! j + 1
        | otherwise =
          pure j
        where
          charCode' :: Int#
          !(I# charCode') = ord charCode

  arrLen <- case needleChars of
    NeedleChars8    needleChars' ->
      T.textFoldIdxM (goAscii (`charMember` needleChars')) 0 haystack

    NeedleChars16   needleChars' ->
      T.textFoldIdxM (goAscii (`charMember` needleChars')) 0 haystack

    NeedleChars24   needleChars' ->
      T.textFoldIdxM (goAscii (`charMember` needleChars')) 0 haystack

    NeedleChars32   needleChars' ->
      T.textFoldIdxM (goAscii (`charMember` needleChars')) 0 haystack

    NeedleCharsLong needleChars' -> do
      let go
            :: StrCharIdx Int
            -> StrByteIdx Int
            -> Char
            -> Int
            -> ST s Int
          go !cidx !bidx !charCode@(C# charCode#) j
            | charMember charCode# needleChars' = do
              let !packedIdxs = mkPackedStrCharIdxAndStrByteIdx (fromIntegral <$> cidx) (fromIntegral <$> bidx)
              UM.unsafeWrite arr' j (CharAndIdxs charCode packedIdxs)
              if isUpper charCode
              then do
                UM.unsafeWrite arr' (j + 1) (CharAndIdxs (toLower charCode) packedIdxs)
                pure $! j + 2
              else
                pure $! j + 1
            | otherwise =
              pure j

      T.textFoldIdxM go 0 haystack

  pure $ UM.unsafeSlice 0 arrLen arr'

isUpperASCII :: Int# -> Bool#
isUpperASCII x = Bool# ((64# <# x) `andI#` (x <# 91#))

toLowerASCII :: Int# -> Int#
toLowerASCII x = x +# 32#

{-# NOINLINE characterOccurrences #-}
-- | For each character in the needle string compute the set of positions
-- it occurs within the haystack.
--
-- Upper-case characters are counted twice as an upper-case and a
-- lower-case character. This is done in order to make lower-case
-- charaters match upper-case ones.
characterOccurrences
  :: ReusableState s
  -> Text -- ^ Needle
  -> NeedleChars
  -> Text -- ^ Haystack
  -> ST s (V.MVector s (U.Vector PackedStrCharIdxAndStrByteIdx), Bool)
characterOccurrences store@ReusableState{rsNeedleStore} !needle !needleChars !haystack = do
  -- rsNeedleStore <- VM.unsafeNew (T.length needle)
  haystackMut <- mkHaystack store needleChars haystack
  sortPackedCharAndIdx haystackMut
  (haystack' :: U.Vector CharAndIdxs) <- U.unsafeFreeze haystackMut
  let
    haystackChars :: U.Vector Char
    haystackIdx   :: U.Vector PackedStrCharIdxAndStrByteIdx
    (!haystackChars, !haystackIdx) = case haystack' of
      V_CharAndIdxs (U.V_2 _ xs ys) -> (xs, ys)

    !haystackLen = U.length haystack'

    findOccurs :: Char -> U.Vector PackedStrCharIdxAndStrByteIdx
    findOccurs !c
      | isMember
      = U.unsafeSlice start (skipSameChars start - start) haystackIdx
      | otherwise
      = U.empty
      where
        (isMember, !start) = VExt.binSearchMemberL c haystackChars

        skipSameChars :: Int -> Int
        skipSameChars !j
          | j == haystackLen
          = j
          | haystackChars `U.unsafeIndex` j == c
          = skipSameChars $ j + 1
          | otherwise
          = j

  !anyEmpty <- T.textFoldIdxM
    (\ !i _ !c (!anyEmpty :: Bool) ->
        if anyEmpty
        then pure anyEmpty
        else do
          let occs :: U.Vector PackedStrCharIdxAndStrByteIdx
              !occs = findOccurs c
          VM.unsafeWrite rsNeedleStore (unStrCharIdx i) occs
          pure $ U.null occs)
    False
    needle
  -- Exposes freezing issue in GHC.
  -- V.unsafeFreeze rsNeedleStore
  pure (VM.unsafeSlice 0 (T.length needle) rsNeedleStore, anyEmpty)

data Match = Match
  { mScore     :: !Int32
  -- | Sorted by construction
  , mPositions :: !(NonEmpty (StrCharIdx Int32))
  } deriving (Eq, Generic, Ord, Show)

data Submatch = Submatch
  { smMatch           :: {-# UNPACK #-} !Match
  , smContiguousCount :: !Int32
  , smMinMaxChar      :: !(MinMaxIdx StrCharIdx)
  , smMinMaxByte      :: !(MinMaxIdx StrByteIdx)
  } deriving (Generic, Show)

fuzzyMatch
  :: forall s. WithCallStack
  => ReusableState s
  -> Heatmap s
  -> Text            -- ^ Needle
  -> Text            -- ^ Haystack
  -> ST s (Maybe Match)
fuzzyMatch store heatmap needle haystack =
  fuzzyMatch' store (pure heatmap) (splitNeedle needle) haystack

instance Semigroup Match where
  Match s1 ps1 <> Match s2 ps2 = Match
    { mScore     = s1 + s2
    , mPositions = merge ps1 ps2
    }
    where
      merge :: Ord a => NonEmpty a -> NonEmpty a -> NonEmpty a
      merge (x :| xs) (y :| ys) = case compare x y of
        LT -> x :| merge' xs (y : ys)
        EQ -> x :| y : merge' xs ys
        GT -> y :| merge' (x : xs) ys
      merge' :: Ord a => [a] -> [a] -> [a]
      merge' []           ys           = ys
      merge' xs           []           = xs
      merge' xs'@(x : xs) ys'@(y : ys) = case compare x y of
        LT -> x : merge' xs ys'
        EQ -> x : y : merge' xs ys
        GT -> y : merge' xs' ys

fuzzyMatch'
  :: forall s. WithCallStack
  => ReusableState s
  -> ST s (Heatmap s)
  -> NonEmpty Text   -- ^ Needle segments to be matched as a conjunction
  -> Text            -- ^ Haystack
  -> ST s (Maybe Match)
fuzzyMatch' store mkHeatmap needleSegments haystack = do
  case needleSegments of
    firstSegment :| otherSegments -> do
      sm <- fuzzyMatchImpl store mkHeatmap firstSegment haystack
      case sm of
        Nothing -> pure Nothing
        Just (sm', heatmap) -> do
          go (smMatch sm') (mkParts 0 sm' haystack heatmap) otherSegments
  where
    mkParts :: Int32 -> Submatch -> Text -> Heatmap s -> NonEmpty (Text, Heatmap s, StrCharIdx Int32)
    mkParts offset Submatch{smMinMaxChar, smMinMaxByte} str heatmap =
      (hk1, hm1, StrCharIdx 0) :| [(hk2, hm2, idx)]
      where
        (hm1, hm2, idx) = splitHeatmap offset smMinMaxChar heatmap
        (hk1, hk2)      = splitHaystack offset smMinMaxByte str

    go :: Match -> NonEmpty (Text, Heatmap s, StrCharIdx Int32) -> [Text] -> ST s (Maybe Match)
    go macc _     []                   = pure $ Just macc
    go macc parts (segment : segments) = do
      matches <- fmap catMaybes $ for (zip [0..] (toList parts)) $ \(i :: Int, part@(haystack', heatmap', _offset)) -> do
        fmap (\(sm, _) -> (i, sm, part))  <$>
          fuzzyMatchImpl store (pure heatmap') segment haystack'
      case matches of
        []     -> pure Nothing
        m : ms -> do
          let !(bestI, bestSM, (bestHaystack, bestHeatmap, StrCharIdx bestOffset)) =
                Foldable1.maximumBy (comparing (\(_i, Submatch{smMatch = Match{mScore}}, _part) -> mScore)) (m :| ms)
              bestMatch :: Match
              bestMatch = (smMatch bestSM) { mPositions = (`charIdxAdvance` bestOffset) <$> mPositions (smMatch bestSM) }
              macc'     = macc <> bestMatch
              parts'    = flip Foldable1.foldMap1 (NE.zip (0 :| [1..]) parts) $ \(i, part) ->
                if i == bestI
                then (\(a, b, c) -> (a, b, charIdxAdvance c bestOffset)) <$> mkParts bestOffset bestSM bestHaystack bestHeatmap
                else part :| []

          go macc' parts' segments

splitHaystack :: Int32 -> MinMaxIdx StrByteIdx -> Text -> (Text, Text)
splitHaystack offset mm (TI.Text arr off len) =
  ( TI.text arr off               bstart'
  , TI.text arr (off + bend' + 1) (len - bend' - 1)
  )
  where
    bstart, bend :: StrByteIdx Int32
    !(!bstart, !bend) = getMinMax mm
    bstart', bend' :: Int
    !bstart' = fromIntegral (unStrByteIdx bstart - offset)
    !bend'   = fromIntegral (unStrByteIdx bend   - offset)

splitHeatmap :: Int32 -> MinMaxIdx StrCharIdx -> Heatmap s -> (Heatmap s, Heatmap s, StrCharIdx Int32)
splitHeatmap offset mm (Heatmap arr) =
  ( Heatmap $ PM.unsafeSlice 0 cstart' arr
  , Heatmap $ PM.unsafeSlice cend' (PM.length arr) arr
  , cend
  )
  where
    cstart, cend :: StrCharIdx Int32
    !(!cstart, !cend) = getMinMax mm
    cstart', cend' :: Int
    !cstart' = fromIntegral (unStrCharIdx cstart - offset)
    !cend'   = fromIntegral (unStrCharIdx cend - offset)


--   matches <- for needleSegments $ \segment -> do
--     sm <- fuzzyMatchImpl store mkHeatmap segment haystack
--     case sm of
--       Nothing -> pure noMatch
--       Just (sm'@Submatch{smMinMaxChar, smMinMaxByte}, heatmap) -> do
--         let bstart, bend :: StrByteIdx Int32
--             (bstart, bend) = getMinMax smMinMaxByte
--             cstart, cend :: StrCharIdx Int32
--             (cstart, cend) = getMinMax smMinMaxChar
--         pure $ submatchToMatch sm'
--   pure $
--     if any (== noMatch) matches
--     then noMatch
--     else Match
--       -- Cannot use product: we have only 21 bit to spare for scores.
--       { mScore     = sum $ map mScore $ toList matches
--       , mPositions = NE.sort $ foldMap1 mPositions matches
--       }

splitNeedle :: Text -> NonEmpty Text
splitNeedle = NE.sortBy (comparing (Dual . T.lengthWord8)) . splitOnSpace
  where
    splitOnSpace :: Text -> NonEmpty Text
    splitOnSpace str = case splitBy (fromIntegral (ord ' ')) str of
      []     -> str :| []
      x : xs -> x :| xs

splitBy :: Word8 -> Text -> [Text]
splitBy c = go
  where
    split str =
      let (# prefix, suffix  #) = T.spanAscii_ (/= c) str
          (# _,      suffix' #) = T.spanAscii_ (== c) suffix
      in (# prefix, suffix' #)
    go str
      | T.null str = []
      | otherwise  =
        let (# prefix, suffix #) = split str
        in if T.null prefix
        then go suffix
        else prefix : go' suffix
    go' str
      | T.null str = []
      | otherwise  =
        let (# prefix, suffix #) = split str
        in prefix : go' suffix

fuzzyMatchImpl
  :: forall s. WithCallStack
  => ReusableState s
  -> ST s (Heatmap s)
  -> Text            -- ^ Needle
  -> Text            -- ^ Haystack
  -> ST s (Maybe (Submatch, Heatmap s))
fuzzyMatchImpl store mkHeatmap needle haystack
  | T.null needle = pure Nothing
  | otherwise     = do
    (occurs :: V.MVector s (U.Vector PackedStrCharIdxAndStrByteIdx), anyEmpty) <- characterOccurrences store needle needleChars haystack
    if anyEmpty -- Also catches occurs == V.empty
    then pure Nothing
    else do

      heatmap'@(Heatmap heatmap) <- unsafeInterleaveST mkHeatmap
      let
        bigger :: StrCharIdx Int32 -> U.Vector PackedStrCharIdxAndStrByteIdx -> U.Vector PackedStrCharIdxAndStrByteIdx
        bigger x xs
          | isMember
          = let !i' = i + 1
            in U.unsafeSlice i' (U.length xs - i') xs
          | otherwise
          = U.unsafeSlice i (U.length xs - i) xs
          where
            (isMember, !i) =
              VExt.binSearchMemberIdx (mkPackedStrCharIdxInLower x) (coerceVectorToPackedStrCharIdxInLower xs)

        computeScore
          :: (V.MVector s (U.Vector PackedStrCharIdxAndStrByteIdx) -> StrCharIdx Int32 -> ST s (Maybe Submatch))
          -> V.MVector s (U.Vector PackedStrCharIdxAndStrByteIdx)
          -> StrCharIdx Int32
          -> ST s (Maybe Submatch)
        computeScore recur !needleOccursInHaystack !cutoffIndex = do
          (remainingOccurrences :: U.Vector PackedStrCharIdxAndStrByteIdx) <-
            bigger cutoffIndex <$> VM.unsafeRead needleOccursInHaystack 0
          case VM.length needleOccursInHaystack of
            -- Last character, already checked that vector is never empty
            1 ->
              inline findBestWith remainingOccurrences $ \ !pidx -> do
                let !(!cidx, !bidx) = unpackIdxs pidx
                heat <- PM.unsafeRead heatmap $ fromIntegral $ unStrCharIdx cidx
                pure $! Just $! Submatch
                  { smMatch           = Match
                    { mScore     = unHeat heat
                    , mPositions = cidx :| []
                    }
                  , smContiguousCount = 0
                  , smMinMaxChar      = mkMinMaxIdx cidx cidx
                  , smMinMaxByte      = mkMinMaxIdx bidx bidx
                  }

            _ ->
              inline findBestWith remainingOccurrences $ \ !pidx -> do
                let !(!cidx, !bidx) = unpackIdxs pidx
                submatch' <- recur (VM.unsafeTail needleOccursInHaystack) cidx
                for submatch' $ \Submatch{smMatch = Match{mScore, mPositions}, smContiguousCount, smMinMaxChar, smMinMaxByte} -> do
                  heat <- PM.unsafeRead heatmap $ fromIntegral $ unStrCharIdx cidx
                  let score'          = mScore + unHeat heat
                      contiguousBonus = 60 + 15 * min 3 smContiguousCount
                      isContiguous    = NE.head mPositions == succ cidx
                      score
                        | isContiguous
                        = score' + contiguousBonus
                        | otherwise
                        = score'
                  pure $ Submatch
                    { smMatch           = Match
                      { mScore     = score
                      , mPositions = NE.cons cidx mPositions
                      }
                    , smContiguousCount =
                      if isContiguous then smContiguousCount + 1 else 0
                    , smMinMaxChar      = mkMinMaxIdx cidx cidx <> smMinMaxChar
                    , smMinMaxByte      = mkMinMaxIdx bidx bidx <> smMinMaxByte
                    }

      fmap (, heatmap') <$> memoizeBy makeKey computeScore occurs (StrCharIdx (-1))
  where
    needleChars = prepareNeedle needle

    makeKey :: V.MVector s (U.Vector a) -> StrCharIdx Int32 -> Int
    makeKey !occs !k =
      j `unsafeShiftL` 32 .|. fromIntegral (unStrCharIdx k)
      where
        !j = VM.length occs

    findBestWith
      :: forall n. Monad n
      => U.Vector PackedStrCharIdxAndStrByteIdx
      -> (PackedStrCharIdxAndStrByteIdx -> n (Maybe Submatch))
      -> n (Maybe Submatch)
    findBestWith !occs f = go Nothing 0
      where
        go :: Maybe Submatch -> Int -> n (Maybe Submatch)
        go !best !i
          | i == U.length occs
          = pure best
          | otherwise
          = do
            x <- f (occs `U.unsafeIndex` i)
            let best' = case (best, x) of
                  (Nothing, y)       -> y
                  (y,       Nothing) -> y
                  (Just b', Just x') ->
                    -- If scores are equal then prefer the match occuring later.
                    Just $! if mScore (smMatch x') >= mScore (smMatch b') then x' else b'
            go best' (i + 1)

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

data Group = Group
  { gPrevChar :: {-# UNPACK #-} !Char
  , gLen      :: {-# UNPACK #-} !Int
  , gStr      :: {-# UNPACK #-} !Text
  -- [gStart, gStart + gLen)
  , gStart    :: {-# UNPACK #-} !(StrCharIdx Int)
  } deriving (Show)

{-# INLINE splitWithSeps #-}
splitWithSeps
  :: Char -- ^ Fake separator to add at the start
  -> PrimArray Int32
  -> Text
  -> Int
  -> Either Group [Group]
splitWithSeps !firstSep !seps !fullStr !fullStrLen
  | sizeofPrimArray seps == 0
  = Left $! Group { gPrevChar = firstSep, gLen = fullStrLen, gStr = fullStr, gStart = StrCharIdx 0 }
  | otherwise
  = Right $! go (StrCharIdx fullStrLen) fullStr
  where
    go :: StrCharIdx Int -> Text -> [Group]
    go !off !str
      | T.null str
      = []
      | T.null prefix
      = if inline isSep $ fromIntegral $ ord $ T.unsafeHead suffix
        then
          [ Group
            { gPrevChar = T.unsafeHead suffix
            , gLen      = len - 1
            , gStr      = T.unsafeTail suffix
            , gStart    = StrCharIdx $ unStrCharIdx off - len + 1
            }
          , Group
            { gPrevChar = firstSep
            , gLen      = 0
            , gStr      = T.empty
            , gStart    = StrCharIdx $ 0
            }
          ]
        else
          [ Group
            { gPrevChar = firstSep
            , gLen      = len
            , gStr      = suffix
            , gStart    = StrCharIdx $ unStrCharIdx off - len
            }
          ]
      | otherwise
      = Group
        { gPrevChar = T.unsafeHead suffix
        , gLen      = len
        , gStr      = T.unsafeTail suffix
        , gStart    = start
        }
      : go (charIdxAdvance start (-1)) prefix
      where
        !start = StrCharIdx $ unStrCharIdx off - len
        isSep  = PExt.binSearchMember seps
        (!len, !prefix, !suffix) = T.spanLenEnd (not . inline isSep . fromIntegral) str

newtype Heat = Heat { unHeat :: Int32 }
  deriving (Eq, Ord, Prim, Pretty, Bounded)

instance Show Heat where
  show = show . unHeat

-- | Heatmap mapping haystack characters to scores.
newtype Heatmap s = Heatmap { unHeatmap :: PM.MVector s Heat }

-- Heatmap elements spast @hastyackLen@ will have undefined values. Take care not
-- to access them!
computeHeatmap :: forall s. ReusableState s -> Text -> Int -> PrimArray Int32 -> ST s (Heatmap s)
computeHeatmap ReusableState{rsHeatmapStore} !haystack !haystackLen groupSeps = do
  arr    <- readSTRef rsHeatmapStore
  scores <- do
    !currSize <- getSizeofMutablePrimArray arr
    if currSize > haystackLen
    then
      pure arr
    else do
      arr' <- resizeMutablePrimArray arr (2 * haystackLen)
      writeSTRef rsHeatmapStore arr'
      pure arr'

  let split = splitWithSeps ' ' groupSeps haystack haystackLen

      !groupsCount = case split of
        Left Group{} -> 1
        Right xs     -> length xs

  let initScore, initAdjustment :: Int
      initScore = (-35)
      !initAdjustment = if groupsCount > 1 then groupsCount * (-2) else 0
      lastCharBonus :: Heat
      !lastCharBonus = Heat 1

  setPrimArray scores 0 haystackLen (Heat (fi32 (initScore + initAdjustment)))
  update (StrCharIdx (haystackLen - 1)) lastCharBonus scores

  let initGroupState = GroupState
        { gsIsBasePath = False
        , gsGroupIdx   = groupsCount - 1
        }

  case split of
    Left  g      -> void $ analyzeGroup g False groupsCount initGroupState haystackLen scores
    Right groups -> goGroups False initGroupState groups
      where
        goGroups :: Bool -> GroupState -> [Group] -> ST s ()
        goGroups !seenBasePath !s = \case
          []     -> pure ()
          g : gs -> do
            s' <- analyzeGroup g seenBasePath groupsCount s haystackLen scores
            goGroups (seenBasePath || gsIsBasePath s') s' gs

  pure $ Heatmap $ PM.MVector 0 haystackLen $ case scores of
    MutablePrimArray xs -> MutableByteArray xs

data GroupState = GroupState
  { gsIsBasePath        :: !Bool
  , gsGroupIdx          :: {-# UNPACK #-} !Int
  } deriving (Show)

data GroupChar = GroupChar
  { gcIsWord      :: Bool#
  , gcIsUpper     :: Bool#
  , gcWordCount   :: {-# UNPACK #-} !Int
  , gcWordIdx     :: {-# UNPACK #-} !Int
  , gcWordCharIdx :: {-# UNPACK #-} !Int
  , gcPrevChar    :: Int#
  }

{-# INLINE fi32 #-}
fi32 :: Integral a => a -> Int32
fi32 = fromIntegral

unST :: State# s -> ST s a -> (# State# s, a #)
unST s (ST f) = f s

newtype Bool# = Bool# Int#

pattern True# :: Bool#
pattern True#  = Bool# 1#

pattern False# :: Bool#
pattern False# = Bool# 0#

toBool :: Bool# -> Bool
toBool (Bool# x) = isTrue# x

toInt :: Bool# -> Int
toInt (Bool# x) = I# x

toInt# :: Bool# -> Int#
toInt# (Bool# x) = x

{-# INLINE bnot# #-}
bnot# :: Bool# -> Bool#
bnot# (Bool# x) = Bool# (1# -# x)

{-# INLINE band# #-}
band# :: Bool# -> Bool# -> Bool#
band# (Bool# x) (Bool# y) = Bool# (andI# x y)

{-# INLINE bor# #-}
bor# :: Bool# -> Bool# -> Bool#
-- bor# x y = (y *# cmp) +# (x *# bnot# cmp)
-- bor# x y = (y *# cmp) +# (x -# x *# cmp)
-- bor# x y = (y *# cmp) +# (x -# x *# cmp)
-- bor# x y = x +# (y *# cmp) -# x *# cmp
-- bor# x y = x +# ((y -# x) *# cmp)
--   where
--     cmp :: Int#
--     cmp = x <# y
 -- bor# = x +# y -# x *# y
bor# (Bool# x) (Bool# y) = Bool# (orI# x y)

isUpper# :: Int# -> Bool#
isUpper# x
  | isTrue# (x <# 128#)
  = isUpperASCII x
  | otherwise
  = if isUpper (chr (I# x)) then Bool# 1# else Bool# 0#

analyzeGroup :: Group -> Bool -> Int -> GroupState -> Int -> MutablePrimArray s Heat -> ST s GroupState
analyzeGroup Group{gPrevChar, gLen, gStr, gStart} !seenBasePath !groupsCount GroupState{gsGroupIdx} !scoresLen !scores = do
  let start :: StrCharIdx Int
      !start = gStart
      end   :: StrCharIdx Int
      !end   = charIdxAdvance start gLen

  let wordStart :: Heat
      !wordStart = Heat 85

  let wordStart#, leadingPenalty# :: Int#
      wordStart#      = 85#
      leadingPenalty# = (-45#)

  GroupChar{gcWordIdx, gcWordCharIdx, gcWordCount} <- ST $ \s -> T.textFoldIntIdxM
    (\ (!idx :: StrCharIdx Int) (c :: Int#) GroupChar{gcIsWord, gcIsUpper, gcWordCount, gcWordIdx, gcWordCharIdx, gcPrevChar} s' -> unST s' $ do

      let currWord, currUpper :: Bool#
          !currWord   = isWord c
          !currUpper  = isUpper# c

          isWord' :: Bool#
          !isWord'    = bnot# gcIsWord `band#` currWord
          isBoundary :: Bool#
          !isBoundary = isWord' `bor#` (bnot# gcIsUpper `band#` currUpper)

          !gcWordIdx'@(I# gcWordIdx'#) = gcWordIdx + toInt isBoundary
            -- | isBoundary = gcWordIdx + 1
            -- | otherwise  = gcWordIdx

          !gcWordCharIdx'@(I# gcWordCharIdx'#) = toInt (bnot# isBoundary) * gcWordCharIdx
            -- | isBoundary = 0
            -- | otherwise  = gcWordCharIdx

          condMul# :: Bool# -> Int# -> Int#
          condMul# x y = toInt# x *# y

          delta :: Heat
          !delta =
            Heat
             (I32#
               (intToInt32#
                 (condMul# isBoundary wordStart# +#
                  condMul# (Bool# (gcWordIdx'# >=# 0#)) (gcWordIdx'# *# (-3#) -# gcWordCharIdx'#) +#
                  condMul# (penaliseIfLeading gcPrevChar) leadingPenalty#)))

            -- GHC pushes update under these ifs and thus wreaks performance.
            -- Heat
            -- $ (if isBoundary then unHeat wordStart else 0)
            -- + (if gcWordIdx' >= 0 then gcWordIdx' * (-3) - gcWordCharIdx' else 0)
            -- + (if penaliseIfLeading gcPrevChar then unHeat leadingPenalty else 0)

      update idx delta scores

      let !gcWordCharIdx'' = gcWordCharIdx' + 1

      pure GroupChar
        { gcIsWord      = currWord
        , gcWordCount   = gcWordCount + toInt isWord'
        , gcIsUpper     = currUpper
        , gcWordIdx     = gcWordIdx'
        , gcWordCharIdx = gcWordCharIdx''
        , gcPrevChar    = c
        })

    (let !(I# prevChar) = ord gPrevChar
     in GroupChar { gcIsWord = isWord prevChar, gcIsUpper = isUpper# prevChar, gcWordCount = 0, gcWordIdx = (-1), gcWordCharIdx = 0, gcPrevChar = prevChar })
    start
    gStr
    s

  when (gStart == StrCharIdx 0 && gLen == 0) $
    update gStart wordStart scores

  -- Update score for trailing separator of current group.
  let !trailingSep = end
  when (unStrCharIdx trailingSep < scoresLen && gcWordIdx >= 0) $
    update trailingSep (Heat $ fi32 $ gcWordIdx * (-3) - gcWordCharIdx) scores

  let !isBasePath = not seenBasePath && gcWordCount /= 0

  let !groupScore = calcGroupScore isBasePath groupsCount gcWordCount gsGroupIdx

  applyGroupScore groupScore start end scoresLen scores

  let res = GroupState
        { gsIsBasePath = isBasePath
        , gsGroupIdx   = gsGroupIdx - 1
        }

  pure res

calcGroupScore :: Bool -> Int -> Int -> Int -> Heat
calcGroupScore isBasePath groupsCount wordCount gcGroupIdx
  | isBasePath = Heat $ fi32 $ 35 + max (groupsCount - 2) 0 - wordCount
  | otherwise  = if gcGroupIdx == 0 then Heat (- 3) else Heat $ fi32 $ gcGroupIdx - 6

penaliseIfLeading :: Int# -> Bool#
penaliseIfLeading x = Bool# (x <=# 46#) `band#` Bool# (x >=# 46#) -- 46 is '.' in ascii

update :: StrCharIdx Int -> Heat -> MutablePrimArray s Heat -> ST s ()
update !idx !val !arr = do
  x <- readPrimArray arr (unStrCharIdx idx)
  writePrimArray arr (unStrCharIdx idx) (coerce ((+) @Int32) x val)

applyGroupScore
  :: forall s. Heat -> StrCharIdx Int -> StrCharIdx Int -> Int -> MutablePrimArray s Heat -> ST s ()
applyGroupScore !score !start !end !arrLen !scores =
  go start
  where
    go :: StrCharIdx Int -> ST s ()
    go !i
      | i < end
      = update i score scores *> go (charIdxAdvance i 1)
      -- If we reached here then i == end
      | unStrCharIdx i < arrLen
      = update i score scores
      | otherwise
      = pure ()
