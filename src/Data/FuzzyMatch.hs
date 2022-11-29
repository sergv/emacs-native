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
{-# LANGUAGE UnliftedFFITypes           #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -Wno-overflowed-literals #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-# OPTIONS_GHC -O2 #-}

{-# OPTIONS_GHC -ddump-simpl -dsuppress-uniques -dsuppress-idinfo -dsuppress-module-prefixes -dsuppress-type-applications -dsuppress-coercions -dppr-cols200 -dsuppress-type-signatures -ddump-to-file #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-uniques -dsuppress-coercions -dppr-cols200 -ddump-to-file #-}
-- {-# OPTIONS_GHC -ddump-spec #-}

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
import Data.Text.Array qualified as TA
import Data.Text.Ext qualified as T
import Data.Text.Internal qualified as TI
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
  { rsHaystackStore :: !(PGM.GrowablePrimArrayMut s Word64)
  , rsNeedleStore   :: !(VM.MVector s (U.Vector PackedIdx))
  }

mkReusableState :: PrimMonad m => Int -> NeedleChars -> m (ReusableState (PrimState m))
mkReusableState needleSize chars = do
  rsHaystackStore <- PGM.new $ needleCharsCountHint chars
  rsNeedleStore   <- VM.unsafeNew needleSize
  pure ReusableState{rsHaystackStore, rsNeedleStore}

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
hasZeroByte !x = 0 < (((x - 0x0101010101010101) .&. complement x .&. 0x8080808080808080))

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
mkHaystack (ReusableState store _) !needleChars !str = do
  -- store <- PGM.new (needleCharsCountHint needleChars)
  (arr, arrLen) <- PGM.with store $ \store' -> do

    let goAscii
          :: (Int# -> Bool)
          -> Int#
          -> Int#
          -> PG.GrowablePrimArrayU ss Word64
          -> State# ss
          -> (# State# ss, PG.GrowablePrimArrayU ss Word64 #)
        goAscii memberPred i charCode arr s1
          | memberPred charCode =
            case PG.pushU (combineCharIdx (W64# c') (I# i)) arr s1 of
              res@(# s2, arr' #) ->
                if isTrue# (isUpperASCII charCode)
                then PG.pushU (combineCharIdx (fromIntegral (I# (toLowerASCII charCode))) (I# i)) arr' s2
                else res
          | otherwise =
            (# s1, arr #)
          where
            c' = wordToWord64# (int2Word# charCode)

    store'' <- case needleChars of
      NeedleChars8    needleChars' -> do
        primitive $ \s ->
          case T.textFoldIdxM' (goAscii (`charMember` needleChars')) (PG.toUnboxed (PG.clear store')) str s of
            (# s2, x #) -> (# s2, PG.fromUnboxed x #)

      NeedleChars16   needleChars' -> do
        primitive $ \s ->
          case T.textFoldIdxM' (goAscii (`charMember` needleChars')) (PG.toUnboxed (PG.clear store')) str s of
            (# s2, x #) -> (# s2, PG.fromUnboxed x #)

      NeedleChars24   needleChars' -> do
        primitive $ \s ->
          case T.textFoldIdxM' (goAscii (`charMember` needleChars')) (PG.toUnboxed (PG.clear store')) str s of
            (# s2, x #) -> (# s2, PG.fromUnboxed x #)

      NeedleChars32   needleChars' -> do
        primitive $ \s ->
          case T.textFoldIdxM' (goAscii (`charMember` needleChars')) (PG.toUnboxed (PG.clear store')) str s of
            (# s2, x #) -> (# s2, PG.fromUnboxed x #)

      NeedleCharsLong needleChars' -> do
        let go
              :: Int#
              -> Int#
              -> PG.GrowablePrimArrayU ss Word64
              -> State# ss
              -> (# State# ss, PG.GrowablePrimArrayU ss Word64 #)
            go i charCode arr s1
              | charMember charCode needleChars' =
                case PG.pushU (combineCharIdx (W64# c') (I# i)) arr s1 of
                  res@(# s2, arr' #) ->
                    if isTrue# (0# /=# iswupper charCode)
                    then PG.pushU (combineCharIdx (fromIntegral (I# (towlower charCode))) (I# i)) arr' s2
                    else res
              | otherwise =
                (# s1, arr #)
              where
                c' = wordToWord64# (int2Word# charCode)

        primitive $ \s ->
          case T.textFoldIdxM' go (PG.toUnboxed (PG.clear store')) str s of
            (# s2, x #) -> (# s2, PG.fromUnboxed x #)

    arr <- PG.finalise store''
    pure ((arr, PG.size store''), store'')

  -- arrLen <- getSizeofMutablePrimArray arr
  pure $ P.MVector 0 arrLen $ PExt.primToByteArr arr

foreign import ccall unsafe "u_towlower"
  towlower :: Int# -> Int#

foreign import ccall unsafe "u_iswupper"
  iswupper :: Int# -> Int#

isUpperASCII :: Int# -> Int#
isUpperASCII x = (64# <# x) `andI#` (x <# 91#)

toLowerASCII :: Int# -> Int#
toLowerASCII x = x +# 32#

{-# INLINE combineCharIdx #-}
combineCharIdx :: Word64 -> Int -> Word64
combineCharIdx c idx = (c `unsafeShiftL` 32) .|. (lower4Bytes .&. w64 idx)

{-# INLINE w64 #-}
w64 :: Integral a => a -> Word64
w64 = fromIntegral

-- {-# INLINE fi64 #-}
-- fi64 :: Integral a => a -> Int64
-- fi64 = fromIntegral

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
  -- Exposes freezing bug in GHC.
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
          1 ->
            pure $ flip map (U.toList remainingOccurrences) $ \pidx ->
              let StrIdx !idx = getStrIdx pidx
              in
              Submatch
                { smScore           = heatmap `indexPrimArray` fromIntegral idx
                , smPositions       = StrIdx idx :| []
                , smContiguousCount = 0
                }

          _ ->
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
computeHeatMapFromGroups haystack (groupsCount, groups) = runPrimArray $ do
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
      $ T.unpack (T.init haystack)

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
    len           = T.length haystack
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
computeGroupsAndInitScores haystack groupSeparators
  | T.null haystack = (0, [])
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
        finalState   = L.foldl' step initState characters
        cs           = T.unpack str
        characters :: [StrIdxCharsInfo]
        characters   = zipWith3 mkStrIdxCharsInfo [succ start..] (prevChar : cs) cs

        initState :: GroupState
        initState = GroupState
          { gsBoundaryIndices = mempty
          , gsWordCount       = 0
          }

        step :: GroupState -> StrIdxCharsInfo -> GroupState
        step GroupState{gsBoundaryIndices, gsWordCount} info =
          GroupState
            { gsBoundaryIndices =
              if haveBoundary info
              then IS.insert (fromIntegral (unStrIdx (infoStrIdx info))) gsBoundaryIndices
              else gsBoundaryIndices
            , gsWordCount       =
              if not (infoPrevIsWord info) && infoCurrIsWord info
              then 1 + gsWordCount
              else gsWordCount
            }

    groupsCount :: Int32
    groups      :: [(StrIdx, Char, StrIdx, Text)]
    ((_, groupsCount), groups)
      = -- filter (\(_, _, len, _) -> len /= 0)
        mapAccumL
          (\(!idx, !len) (sep, str') ->
            let !next = idx + T.length str' in
            ((next + 1, len + 1), (StrIdx (fromIntegral idx), sep, StrIdx (fromIntegral next), str')))
          ((-1) -- To account for fake separator at the beginning
          , 0
          )
      $ splitWithSeps ' ' groupSeparators haystack

    -- Check whetehr @lastChar@ is the end of a word and
    -- @currentChar@ is the start of the next.
    haveBoundary :: StrIdxCharsInfo -> Bool
    haveBoundary info =
      not (infoPrevIsUpper info) && infoCurrIsUpper info ||
      not (infoPrevIsWord info) && infoCurrIsWord info

-- Isomorphic to (StrIdx, (Bool, Bool) , (Bool, Bool)) derived from (StrIdx, Char, Char)>
newtype StrIdxCharsInfo = StrIdxCharsInfo { unStrIdxCharsInfo :: Word64 }

instance Show StrIdxCharsInfo where
  showsPrec _ = (showString "0x" .) . showHex . unStrIdxCharsInfo

mkStrIdxCharsInfo :: StrIdx -> Char -> Char -> StrIdxCharsInfo
mkStrIdxCharsInfo (StrIdx idx) prev curr = StrIdxCharsInfo $
  fromIntegral idx .|.
  mkBit 32 prevWord .|.
  mkBit 33 currWord .|.
  mkBit 34 prevUpper .|.
  mkBit 35 currUpper
  where
    prevWord  = isWord prev
    currWord  = isWord curr
    prevUpper = isUpper prev
    currUpper = isUpper curr
    mkBit :: Int -> Bool -> Word64
    mkBit !n !x = if x then 1 `unsafeShiftL` n else 0

infoStrIdx :: StrIdxCharsInfo -> StrIdx
infoStrIdx (StrIdxCharsInfo x) = StrIdx $ fromIntegral $ lower4Bytes .&. x

infoPrevIsWord :: StrIdxCharsInfo -> Bool
infoPrevIsWord (StrIdxCharsInfo x) = (x `unsafeShiftR` 32 .&. 0x1) == 1

infoCurrIsWord :: StrIdxCharsInfo -> Bool
infoCurrIsWord (StrIdxCharsInfo x) = (x `unsafeShiftR` 33 .&. 0x1) == 1

infoPrevIsUpper :: StrIdxCharsInfo -> Bool
infoPrevIsUpper (StrIdxCharsInfo x) = (x `unsafeShiftR` 34 .&. 0x1) == 1

infoCurrIsUpper :: StrIdxCharsInfo -> Bool
infoCurrIsUpper (StrIdxCharsInfo x) = (x `unsafeShiftR` 35 .&. 0x1) == 1
