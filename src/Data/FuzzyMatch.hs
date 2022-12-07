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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UnliftedFFITypes           #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -O2 #-}

module Data.FuzzyMatch
  ( fuzzyMatch'
  , fuzzyMatch
  , Match(..)
  , NeedleChars
  , prepareNeedle
  , ReusableState
  , mkReusableState

  -- * Interface for testing
  , computeHeatmap
  -- , computeGroupsAndInitScores
  , Heatmap(..)
  , Heat(..)
  -- , HeatmapGroup(..)
  , StrIdx(..)
  ) where

import Control.Monad
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
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
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
import Data.Text.Unsafe qualified as T
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
import GHC.Word
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
  { rsHaystackStore :: !(STRef s (MutableByteArray s))
  , rsHeatmapStore  :: !(STRef s (PM.MVector s Heat))
  , rsNeedleStore   :: !(VM.MVector s (U.Vector PackedIdx))
  }

mkReusableState :: Int -> NeedleChars -> ST s (ReusableState s)
mkReusableState needleSize chars = do
  rsHaystackStore <- newSTRef =<< newByteArray (needleCharsCountHint chars)
  rsHeatmapStore  <- newSTRef =<< PM.unsafeNew (needleCharsCountHint chars)
  rsNeedleStore   <- VM.unsafeNew needleSize
  pure ReusableState{rsHaystackStore, rsHeatmapStore, rsNeedleStore}

newtype Bytes8 = Bytes8 Word64
data Bytes16 = Bytes16 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
data Bytes24 = Bytes24 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
data Bytes32 = Bytes32 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
newtype CharsVector = CharsVector { unCharsVector :: P.Vector Char }

class CharMember a where
  charMember :: Int# -> a -> Bool

instance CharMember Bytes8 where
  charMember charCode (Bytes8 w1) = isTrue# (charCode <# 128#) && wordMember w1 (fanout charCode)

instance CharMember Bytes16 where
  charMember charCode (Bytes16 w1 w2) = isTrue# (charCode <# 128#) && (wordMember w1 bytes || wordMember w2 bytes)
    where
      !bytes = fanout charCode

instance CharMember Bytes24 where
  charMember charCode (Bytes24 w1 w2 w3) = isTrue# (charCode <# 128#) && (wordMember w1 bytes || wordMember w2 bytes || wordMember w3 bytes)
    where
      !bytes = fanout charCode

instance CharMember Bytes32 where
  charMember charCode (Bytes32 w1 w2 w3 w4) = isTrue# (charCode <# 128#) && (wordMember w1 bytes || wordMember w2 bytes || wordMember w3 bytes ||  wordMember w4 bytes)
    where
      !bytes = fanout charCode

instance CharMember CharsVector where
  charMember charCode (CharsVector arr) = VExt.binSearchMember (C# (chr# charCode)) arr

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

needleCharsCountHint :: NeedleChars -> Int
needleCharsCountHint = \case
  NeedleChars8 _     -> 8
  NeedleChars16 _    -> 16
  NeedleChars24 _    -> 24
  NeedleChars32 _    -> 32
  NeedleCharsLong xs -> P.length (unCharsVector xs)

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

newtype PackedCharAndIdx = PackedCharAndIdx { _unPackedCharAndIdx :: Word64 }

newtype instance U.MVector s PackedCharAndIdx = MV_PackedCharAndIdx (U.MVector s Word64)
newtype instance U.Vector    PackedCharAndIdx = V_PackedCharAndIdx  (U.Vector    Word64)
deriving instance GM.MVector U.MVector PackedCharAndIdx
deriving instance G.Vector   U.Vector  PackedCharAndIdx
instance U.Unbox PackedCharAndIdx

instance Show PackedCharAndIdx where
  show (PackedCharAndIdx x) =
    show
      ( chr $ fromIntegral $ keepChar (PackedChar x) `unsafeShiftR` 32
      , showString "0x" . showHex (keepChar (PackedChar x) `unsafeShiftR` 32) $ []
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
mkHaystack ReusableState{rsHaystackStore} !needleChars !str@(TI.Text _ _ haystackBytes) = do
  -- store <- PGM.new (needleCharsCountHint needleChars)
  arr <- readSTRef rsHaystackStore

  let toReserve = 2 * haystackBytes * I# (sizeOf# (undefined :: Word64))

  currSize <- getSizeofMutableByteArray arr

  arr'@(MutableByteArray mbarr) <-
    if toReserve <= currSize
    then pure arr
    else do
      arr' <- resizeMutableByteArray arr toReserve
      writeSTRef rsHaystackStore arr'
      pure arr'

  let goAscii
        :: (Int# -> Bool)
        -> Word64
        -> Int#
        -> Int#
        -> State# s
        -> (# State# s, Int# #)
      goAscii memberPred i charCode j s1
        | memberPred charCode =
          case writeByteArray# mbarr j (combineCharIdx (W64# c') i) s1 of
            s2 ->
              if isTrue# (isUpperASCII charCode)
              then
                case writeByteArray# mbarr (j +# 1#) (combineCharIdx (fromIntegral (I# (toLowerASCII charCode))) i) s2 of
                  s3 -> (# s3, j +# 2# #)
              else (# s2, j +# 1# #)
        | otherwise =
          (# s1, j #)
        where
          c' = wordToWord64# (int2Word# charCode)

  arrLen <- case needleChars of
    NeedleChars8    needleChars' -> do
      primitive $ \s ->
        case T.textFoldIdxM' (goAscii (`charMember` needleChars')) 0# str s of
          (# s2, arrLen #) -> (# s2, I# arrLen #)

    NeedleChars16   needleChars' -> do
      primitive $ \s ->
        case T.textFoldIdxM' (goAscii (`charMember` needleChars')) 0# str s of
          (# s2, arrLen #) -> (# s2, I# arrLen #)

    NeedleChars24   needleChars' -> do
      primitive $ \s ->
        case T.textFoldIdxM' (goAscii (`charMember` needleChars')) 0# str s of
          (# s2, arrLen #) -> (# s2, I# arrLen #)

    NeedleChars32   needleChars' -> do
      primitive $ \s ->
        case T.textFoldIdxM' (goAscii (`charMember` needleChars')) 0# str s of
          (# s2, arrLen #) -> (# s2, I# arrLen #)

    NeedleCharsLong needleChars' -> do
      let go
            :: Word64
            -> Int#
            -> Int#
            -> State# s
            -> (# State# s, Int# #)
          go i charCode j s1
            | charMember charCode needleChars' =
              case writeByteArray# mbarr j (combineCharIdx (W64# c') i) s1 of
                s2 ->
                  if isTrue# (0# /=# iswupper charCode)
                  then
                    case writeByteArray# mbarr (j +# 1#) (combineCharIdx (fromIntegral (I# (towlower charCode))) i) s2 of
                      s3 -> (# s3, j +# 2# #)
                  else (# s2, j +# 1# #)
            | otherwise =
              (# s1, j #)
            where
              c' = wordToWord64# (int2Word# charCode)

      primitive $ \s ->
        case T.textFoldIdxM' go 0# str s of
          (# s2, arrLen #) -> (# s2, I# arrLen #)

  pure $ P.MVector 0 arrLen arr'

foreign import ccall unsafe "u_towlower"
  towlower :: Int# -> Int#

foreign import ccall unsafe "u_iswupper"
  iswupper :: Int# -> Int#

isUpperASCII :: Int# -> Int#
isUpperASCII x = (64# <# x) `andI#` (x <# 91#)

toLowerASCII :: Int# -> Int#
toLowerASCII x = x +# 32#

{-# INLINE combineCharIdx #-}
combineCharIdx :: Word64 -> Word64 -> Word64
-- Safe to omit anding with lower4Bytes because index is unlikely to reach a point where that
-- operation would have any effect
-- combineCharIdx c idx = (c `unsafeShiftL` 32) .|. (lower4Bytes .&. w64 idx)
combineCharIdx c idx = (c `unsafeShiftL` 32) .|. idx

{-# INLINE w64 #-}
w64 :: Integral a => a -> Word64
w64 = fromIntegral

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
characterOccurrences store@ReusableState{rsNeedleStore} !needle !needleChars !haystack = do
  -- rsNeedleStore <- VM.unsafeNew (T.length needle)
  haystackMut <- mkHaystack store needleChars haystack
  {-# SCC "haystack-sort" #-} qsortWord64 haystackMut
  (haystack' :: U.Vector Word64) <- U.V_Word64 <$> P.unsafeFreeze haystackMut

  let
    haystackChars :: U.Vector PackedChar
    !haystackChars = coerce haystack'

    haystackIdx :: U.Vector PackedIdx
    !haystackIdx = coerce haystack'

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

  !anyEmpty <- T.textFoldIdxM
    (\ !i !c (!anyEmpty :: Bool) ->
        if anyEmpty
        then pure anyEmpty
        else do
          let !occs = findOccurs c
          VM.unsafeWrite rsNeedleStore i occs
          pure $ U.null occs)
    False
    needle
  -- Exposes freezing bug in GHC.
  -- V.unsafeFreeze rsNeedleStore
  pure (rsNeedleStore, anyEmpty)

data Match = Match
  { mScore     :: !Int32
  , mPositions :: !(NonEmpty StrIdx)
  } deriving (Eq, Generic, Ord, Show)

data Submatch = Submatch
  { smScore           :: !Heat
  , smPositions       :: !(NonEmpty StrIdx)
  , smContiguousCount :: !Int32
  } deriving (Generic, Show)

submatchToMatch :: Submatch -> Match
submatchToMatch Submatch{smScore, smPositions} = Match
  { mScore     = unHeat smScore
  , mPositions = smPositions
  }

fuzzyMatch
  :: forall s. WithCallStack
  => ReusableState s
  -> Heatmap
  -> Text            -- ^ Needle
  -> NeedleChars     -- ^ Sorted needle characters
  -> Text            -- ^ Haystack
  -> ST s Match
fuzzyMatch store heatmap needle needleChars haystack =
  fuzzyMatch' store (pure heatmap) needle needleChars haystack

fuzzyMatch'
  :: forall s. WithCallStack
  => ReusableState s
  -> ST s Heatmap
  -> Text            -- ^ Needle
  -> NeedleChars     -- ^ Sorted needle characters
  -> Text            -- ^ Haystack
  -> ST s Match
fuzzyMatch' store mkHeatmap needle needleChars haystack
  | T.null needle = pure noMatch
  | otherwise     = do
    (occurs :: V.MVector s (U.Vector PackedIdx), anyEmpty) <- characterOccurrences store needle needleChars haystack
    if anyEmpty -- Also catches occurs == V.empty
    then pure noMatch
    else do

      Heatmap heatmap <- unsafeInterleaveST mkHeatmap
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
            1 ->
              pure $ flip map (U.toList remainingOccurrences) $ \pidx ->
                let StrIdx !idx = getStrIdx pidx
                in
                Submatch
                  { smScore           = heatmap `P.unsafeIndex` fromIntegral idx
                  , smPositions       = StrIdx idx :| []
                  , smContiguousCount = 0
                  }

            _ ->
              fmap (getMaximum . concat) $ for (U.toList remainingOccurrences) $ \pidx -> do
                let !idx' = getStrIdx pidx
                submatches <- recur (VM.unsafeTail needleOccursInHaystack) pidx
                pure $ getMaximum $ flip map submatches $ \submatch ->
                  let score'          = smScore submatch + (heatmap `P.unsafeIndex` fromIntegral (unStrIdx idx'))
                      contiguousBonus = Heat $ 60 + 15 * min 3 (smContiguousCount submatch)
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

      !result <- do
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
      j `unsafeShiftL` 32 .|. fromIntegral (unStrIdx (keepIdx k))
      where
        !j = VM.length occs

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

-- data HeatmapGroup = HeatmapGroup
--   { -- | At which index the group starts, inclusive. Usually points to
--     -- separator that started the group, even for the first group where
--     -- it's equal to -1. So, w.r.t. interesting group contents this index
--     -- is exclusive.
--     hmgStart           :: !StrIdx
--     -- | At which index the group ends, inclusive.
--   , hmgEnd             :: !StrIdx
--   , hmgWordCount       :: !Int32
--     -- | Word indices
--   , hmgWordIndices     :: ![StrIdx]
--   , hmgWordIndicesSize :: !Int32
--   , hmgIsBasePath      :: !Bool
--   } deriving (Eq, Ord, Show, Generic)

data Group = Group
  { gPrevChar :: {-# UNPACK #-} !Char
  , gLen      :: {-# UNPACK #-} !Int
  , gStr      :: {-# UNPACK #-} !Text
  -- [gStart, gStart + gLen)
  , gStart    :: {-# UNPACK #-} !Int
  } deriving (Show)

splitWithSeps
  :: Char -- ^ Fake separator to add at the start
  -> PrimArray Int32
  -> Text
  -> Int
  -> Either Group [Group]
splitWithSeps !firstSep !seps !fullStr !fullStrLen
  | sizeofPrimArray seps == 0
  = Left $ Group { gPrevChar = firstSep, gLen = fullStrLen, gStr = fullStr, gStart = 0 }
  | otherwise
  = Right $ go fullStrLen fullStr
  where
    go :: Int -> Text -> [Group]
    go !off !str
      | T.null str
      = []
      | T.null prefix
      = [ Group
          { gPrevChar = firstSep
          , gLen      = len
          , gStr      = suffix
          , gStart    = off - len
          }
        ]
      | otherwise
      = Group
        { gPrevChar = T.unsafeHead suffix
        , gLen      = len
        , gStr      = T.unsafeTail suffix
        , gStart    = start
        }
      : go (start - 1) prefix
      where
        !start = off - len
        (len, prefix, suffix) = T.spanLenEnd (not . PExt.binSearchMember seps . fromIntegral) str

newtype Heat = Heat { unHeat :: Int32 }
  deriving (Eq, Ord, Num, Prim)

instance Show Heat where
  show = show . unHeat

-- | Heatmap mapping characters to scores
newtype Heatmap = Heatmap { unHeatmap :: P.Vector Heat }
  deriving (Show)

computeHeatmap :: ReusableState s -> Text -> PrimArray Int32 -> ST s Heatmap
computeHeatmap ReusableState{rsHeatmapStore} haystack groupSeps = do
  vec    <- readSTRef rsHeatmapStore
  scores <- do
    let !currSize = PM.length vec
    vec' <-
      if currSize > len
      then pure vec
      else do
        vec' <- PM.unsafeNew (len * 2)
        writeSTRef rsHeatmapStore vec'
        pure vec'
    pure $ PM.unsafeSlice 0 len vec'

  let split = splitWithSeps ' ' groupSeps haystack len

      !groupsCount = case split of
        Left Group{} -> 1
        Right xs     -> length xs

  let initScore, initAdjustment :: Heat
      initScore = (-35)
      !initAdjustment = if groupsCount > 1 then fromIntegral groupsCount * (-2) else 0
      lastCharBonus :: Heat
      !lastCharBonus = 1

  PM.set scores (initScore + initAdjustment)
  update (len - 1) lastCharBonus scores

  let initGroupState = GroupState
        { gsIsBasePath = False
        , gsGroupIdx   = fi32 $ groupsCount - 1
        }
  case split of
    Left  g      -> void $ analyzeGroup g False groupsCount initGroupState scores
    Right groups -> goGroups False initGroupState groups
      where
        goGroups !seenBasePath !s = \case
          []     -> pure ()
          g : gs -> do
            s' <- analyzeGroup g seenBasePath groupsCount s scores
            goGroups (seenBasePath || gsIsBasePath s') s' gs

  Heatmap <$> P.unsafeFreeze scores

  where
    !len = T.length haystack

data GroupState = GroupState
  { gsIsBasePath        :: !Bool
  , gsGroupIdx          :: {-# UNPACK #-} !Int32
  -- , gsGroupNonBaseScore :: {-# UNPACK #-} !Heat
  } deriving (Show)

data GroupChar = GroupChar
  { gcIsWord      :: !Bool
  , gcIsUpper     :: !Bool
  , gcWordCount   :: {-# UNPACK #-} !Int
  , gcWordIdx     :: {-# UNPACK #-} !Int
  , gcWordCharIdx :: {-# UNPACK #-} !Int
  } deriving (Show)

{-# INLINE fi32 #-}
fi32 :: Integral a => a -> Int32
fi32 = fromIntegral

analyzeGroup :: Group -> Bool -> Int -> GroupState -> PM.MVector s Heat -> ST s GroupState
analyzeGroup Group{gPrevChar, gLen, gStr, gStart} seenBasePath groupsCount GroupState{gsGroupIdx} scores = do
  let start :: Int
      !start = gStart
      !end = start + gLen

  let wordStart, leadingPenalty :: Heat
      !wordStart = 85
      !leadingPenalty = (-45)

  GroupChar{gcWordIdx, gcWordCharIdx, gcWordCount} <- T.textFoldIdxM
    (\ !idx !c GroupChar{gcIsWord, gcIsUpper, gcWordCount, gcWordIdx, gcWordCharIdx} -> do
      let j :: Int
          !j = start + idx

          !currWord   = isWord c
          !currUpper  = isUpper c

          !isWord'    = not gcIsWord && currWord
          !isBoundary = isWord' || not gcIsUpper && currUpper

      let (!gcWordIdx', !gcWordCharIdx')
            | isBoundary = (gcWordIdx + 1, 0)
            | otherwise  = (gcWordIdx, gcWordCharIdx)
      when isBoundary $
        update j wordStart scores

      when (gcWordIdx' >= 0) $
        update j (Heat $ fi32 $ gcWordIdx' * (-3) - gcWordCharIdx') scores

      let !gcWordCharIdx'' = gcWordCharIdx' + 1

      when (penalisedIfLeading c) $ do
        let !k = j + 1
        when (k < PM.length scores) $
          update k leadingPenalty scores

      pure GroupChar
        { gcIsWord      = currWord
        , gcWordCount   = if isWord' then gcWordCount + 1 else gcWordCount
        , gcIsUpper     = currUpper
        , gcWordIdx     = gcWordIdx'
        , gcWordCharIdx = gcWordCharIdx''
        })
    (GroupChar { gcIsWord = isWord gPrevChar, gcIsUpper = isUpper gPrevChar, gcWordCount = 0, gcWordIdx = (-1), gcWordCharIdx = 0 })
    gStr

  -- Update score for trailing separator of current group.
  let !trailingSep = end
  when (trailingSep < PM.length scores && gcWordIdx >= 0) $
    update trailingSep (Heat $ fi32 $ gcWordIdx * (-3) - gcWordCharIdx) scores

  let !isBasePath = not seenBasePath && gcWordCount /= 0

  let !groupScore = calcGroupScore isBasePath groupsCount gcWordCount gsGroupIdx

  applyGroupScore groupScore start end scores

  let res = GroupState
        { gsIsBasePath = isBasePath
        , gsGroupIdx   = gsGroupIdx - 1
        }

  pure res

calcGroupScore :: Bool -> Int -> Int -> Int32 -> Heat
calcGroupScore isBasePath groupsCount wordCount gcGroupIdx
  | isBasePath = Heat $ fi32 $ 35 + max (groupsCount - 2) 0 - wordCount
  | otherwise  = if gcGroupIdx == 0 then (- 3) else Heat $ gcGroupIdx - 6

penalisedIfLeading :: Char -> Bool
penalisedIfLeading = (== '.')

update :: Int -> Heat -> PM.MVector s Heat -> ST s ()
update !idx !val vec =
  PM.unsafeModify vec (+ val) idx

applyGroupScore :: Heat -> Int -> Int -> PM.MVector s Heat -> ST s ()
applyGroupScore !score !start !end !scores =
  go start
  where
    go !i
      | i < end
      = update i score scores *> go (i + 1)
      -- i == end
      | i < PM.length scores
      = update i score scores
      | otherwise
      = pure ()



-- computeHeatmap :: ReusableState s -> Text -> PrimArray Int32 -> ST s Heatmap
-- computeHeatmap store haystack =
--   computeHeatmapFromGroups store haystack len . computeGroupsAndInitScores haystack len
--   where
--     !len = T.length haystack

-- computeHeatmapFromGroups :: ReusableState s -> Text -> Int -> (Int32, [HeatmapGroup]) -> ST s Heatmap
-- computeHeatmapFromGroups ReusableState{rsHeatmapStore} !haystack !len (!groupsCount, groups) = do
--   vec      <- readSTRef rsHeatmapStore
--   scores   <- do
--     let !currSize = PM.length vec
--     vec' <-
--       if currSize > len
--       then pure vec
--       else do
--         vec' <- PM.unsafeNew (len * 2)
--         writeSTRef rsHeatmapStore vec'
--         pure vec'
--     pure $ PM.unsafeSlice 0 len vec'
--
--   -- scores <- newPrimArray len
--
--   PM.set scores (initScore + initScoreAdjustment)
--   update lastCharIdx lastCharBonus scores
--   applyGroupScores scores groupScores
--   addWordScores scores indexedWords
--   -- Apply penalties
--   T.textForIdx_ (T.init haystack) $ \ !idx !c ->
--     when (penalisedIfLeading c) $ do
--       -- Add 1 to index since we've skipped past first character of haystack.
--       let !idx' = StrIdx (fromIntegral (idx + 1))
--       update idx' (-45) scores
--
--   Heatmap <$> P.unsafeFreeze scores
--   where
--     groupScores :: [(HeatmapGroup, Int32)]
--     groupScores = annotate groups
--       where
--         annotate :: [HeatmapGroup] -> [(HeatmapGroup, Int32)]
--         annotate []       = []
--         annotate (g : gs) = (g, score) : annotate' (-5) gs
--           where
--             !score = groupBasicScore (-3) g
--
--         annotate' :: Int32 -> [HeatmapGroup] -> [(HeatmapGroup, Int32)]
--         annotate' _  []       = []
--         annotate' !k (g : gs) = (g, score) : annotate' (k + 1) gs
--           where
--             !score = groupBasicScore k g
--
--       -- zipWith (\d g -> (g, groupBasicScore d g)) (-3 : iterate (+ 1) (-5)) groups
--
--     applyGroupScores :: forall ss. PM.MVector ss Int32 -> [(HeatmapGroup, Int32)] -> ST ss ()
--     applyGroupScores scores =
--       traverse_ $ \(HeatmapGroup{hmgStart, hmgEnd}, score) ->
--         forFromTo_ (succ hmgStart) (min lastCharIdx (succ hmgEnd)) $ \_ pos ->
--           update pos score scores
--
--     indexedWords :: [(StrIdx, StrIdx, Int32)]
--     indexedWords =
--       fst $
--       foldr
--         (\HeatmapGroup{hmgWordIndices, hmgWordIndicesSize, hmgStart} (results, end) ->
--           let newIndices :: [(StrIdx, StrIdx, Int32)]
--               newIndices = go (hmgWordIndicesSize - 1) end hmgWordIndices
--                where
--                  go !n !end' = \case
--                    []             -> results
--                    wordStart : ws -> (wordStart, end', n) : go (n - 1) (pred wordStart) ws
--           in (newIndices, hmgStart))
--         ([], lastCharIdx)
--         groups
--
--     addWordScores :: forall ss. PM.MVector ss Int32 -> [(StrIdx, StrIdx, Int32)] -> ST ss ()
--     addWordScores scores = traverse_ applyScores
--       where
--         applyScores (start, end, wordIdx) = do
--           update start 85 scores
--           forFromTo_ start end $ \wordCharIdx pos ->
--             update pos (wordIdx * (-3) - wordCharIdx) scores
--
--     penalisedIfLeading :: Char -> Bool
--     penalisedIfLeading = (== '.')
--
--     initScoreAdjustment :: Int32
--     initScoreAdjustment = case groups of
--       []  -> 0
--       [_] -> 0
--       _   -> (-2) * groupsCount
--
--     update :: forall ss. StrIdx -> Int32 -> PM.MVector ss Int32 -> ST ss ()
--     update (StrIdx idx) !val vec =
--       PM.unsafeModify vec (+ val) (fromIntegral idx)
--
--     forFromTo_ :: Monad m => StrIdx -> StrIdx -> (Int32 -> StrIdx -> m ()) -> m ()
--     forFromTo_ start end f = go 0
--       where
--         !count = unStrIdx end - unStrIdx start
--         go !i = do
--           f i (StrIdx (coerce start + i))
--           if i == count
--           then pure ()
--           else go (i + 1)
--
--     initScore, lastCharBonus :: Int32
--     initScore     = (-35)
--     lastCharBonus = 1
--     lastCharIdx   = StrIdx $ fromIntegral $ len - 1
--
--     groupBasicScore :: Int32 -> HeatmapGroup -> Int32
--     groupBasicScore nonBasePathDelta HeatmapGroup{hmgIsBasePath, hmgWordCount}
--       | hmgIsBasePath = 35 + max (groupsCount - 2) 0 - hmgWordCount
--       | otherwise     = nonBasePathDelta
--
--
-- data GroupState = GroupState
--   { gsBoundaryIndices :: ![StrIdx]
--   , gsBoundarySize    :: !Int32
--   , gsWordCount       :: !Int32
--   }
--
-- computeGroupsAndInitScores :: Text -> Int -> PrimArray Int32 -> (Int32, [HeatmapGroup])
-- computeGroupsAndInitScores !haystack !haystackLen !groupSeparators
--   | T.null haystack = (0, [])
--   | otherwise
--   = (groupsCount, )
--   $ fst
--   $ foldr (\x@HeatmapGroup{hmgIsBasePath} (xs, seenBasePath) ->
--              (x { hmgIsBasePath = not seenBasePath && hmgIsBasePath } : xs, seenBasePath || hmgIsBasePath))
--           ([], False)
--   $ map analyseGroup groups
--   where
--     analyseGroup :: (StrIdx, Char, StrIdx, Text) -> HeatmapGroup
--     analyseGroup (start, prevChar, end, str) =
--       HeatmapGroup
--         { hmgStart           = start
--         , hmgEnd             = end
--         , hmgWordCount
--         , hmgWordIndices     = gsBoundaryIndices finalState
--         , hmgWordIndicesSize = gsBoundarySize finalState
--         , hmgIsBasePath      = hmgWordCount /= 0
--         }
--       where
--         hmgWordCount = gsWordCount finalState
--
--         (finalState, _, _) = T.textFoldIdx step' (initState, isWord prevChar, isUpper prevChar) str
--
--         step' :: Int -> Char -> (GroupState, Bool, Bool) -> (GroupState, Bool, Bool)
--         step' i c (GroupState{gsBoundaryIndices, gsBoundarySize, gsWordCount}, prevWord, prevUpper) =
--           (nextState, currWord, currUpper)
--           where
--             currWord  = isWord c
--             currUpper = isUpper c
--
--             -- Check whether @lastChar@ is the end of a word and
--             -- @currentChar@ is the start of the next.
--             haveBoundary = not prevUpper && currUpper || not prevWord && currWord
--
--             !idx' = StrIdx $ fromIntegral i + (unStrIdx start + 1)
--
--             nextState = GroupState
--               { gsBoundaryIndices =
--                 if haveBoundary
--                 then idx' : gsBoundaryIndices
--                 else gsBoundaryIndices
--               , gsBoundarySize    =
--                 if haveBoundary
--                 then 1 + gsBoundarySize
--                 else gsBoundarySize
--               , gsWordCount       =
--                 if not prevWord && currWord
--                 then 1 + gsWordCount
--                 else gsWordCount
--               }
--
--         initState :: GroupState
--         initState = GroupState
--           { gsBoundaryIndices = mempty
--           , gsBoundarySize    = 0
--           , gsWordCount       = 0
--           }
--
--     groupsCount :: Int32
--     groups      :: [(StrIdx, Char, StrIdx, Text)]
--     ((_, groupsCount), groups)
--       = -- filter (\(_, _, len, _) -> len /= 0)
--         mapAccumL
--           (\(!idx, !len) (!sep, !groupLen, !str') ->
--             let !next = idx + groupLen in
--             ((next + 1, len + 1), (StrIdx (fromIntegral idx), sep, StrIdx (fromIntegral next), str')))
--           ((-1) -- To account for fake separator at the beginning
--           , 0
--           )
--       $ splitWithSeps ' ' groupSeparators haystack haystackLen
