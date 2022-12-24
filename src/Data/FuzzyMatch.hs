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
  , Heatmap(..)
  , Heat(..)
  , StrIdx(..)
  ) where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST.Strict
import Data.Bits
import Data.Char
import Data.Coerce
import Data.Function
import Data.Int
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
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
import Prettyprinter
import Prettyprinter.Show

import Emacs.Module.Assert (WithCallStack)

-- {-# INLINE isWordSeparator #-}
-- isWordSeparator :: Char -> Bool
-- isWordSeparator = \case
--   ' '  -> True
--   '\t' -> True
--   '\r' -> True
--   '\n' -> True
--   '*'  -> True
--   '+'  -> True
--   '-'  -> True
--   '_'  -> True
--   ':'  -> True
--   ';'  -> True
--   '.'  -> True
--   ','  -> True
--   '/'  -> True
--   '\\' -> True
--   _    -> False

-- {-# INLINE isWordSeparator #-}
-- isWordSeparator :: Char -> Bool
-- isWordSeparator c =
--   not $ '0' <= c && c <= '9' || 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z'

wordSeps :: Bytes16
wordSeps = Bytes16
  (c8 ' ' '\t' '\r' '\n' '*' '+' '-' '_')
  (c8 ':' ';' '.' ',' '/' '\\' ' ' ' ')
  where
    c8 :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Word64
    c8 !x1 !x2 !x3 !x4 !x5 !x6 !x7 !x8 =
      w8
        (fromIntegral (ord x1))
        (fromIntegral (ord x2))
        (fromIntegral (ord x3))
        (fromIntegral (ord x4))
        (fromIntegral (ord x5))
        (fromIntegral (ord x6))
        (fromIntegral (ord x7))
        (fromIntegral (ord x8))

    w8 :: Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64
    w8 !x1 !x2 !x3 !x4 !x5 !x6 !x7 !x8 =
      x1 .|.
      x2 `unsafeShiftL` 8 .|.
      x3 `unsafeShiftL` 16 .|.
      x4 `unsafeShiftL` 24 .|.
      x5 `unsafeShiftL` 32 .|.
      x6 `unsafeShiftL` 40 .|.
      x7 `unsafeShiftL` 48 .|.
      x8 `unsafeShiftL` 56

{-# INLINE isWordSeparator #-}
isWordSeparator :: Int# -> Bool
isWordSeparator x = charMember x wordSeps

{-# INLINE isWord #-}
isWord :: Int# -> Bool
isWord x = not(isWordSeparator x)

{-# INLINE isWordC #-}
isWordC :: Char -> Bool
isWordC c = isWord x
  where
    !(I# x) = ord c

data ReusableState s = ReusableState
  { rsHaystackStore :: !(STRef s (MutableByteArray s))
  , rsHeatmapStore  :: !(STRef s (PM.MVector s Heat))
  , rsNeedleStore   :: !(VM.MVector s (U.Vector PackedIdx))
  }

mkReusableState :: Int -> NeedleChars -> ST s (ReusableState s)
mkReusableState !needleSize !chars = do
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
  qsortWord64 haystackMut
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
        bigger :: StrIdx -> U.Vector PackedIdx -> U.Vector PackedIdx
        bigger x xs
          | isMember
          = let !i' = i + 1
            in U.unsafeSlice i' (U.length xs - i') xs
          | otherwise
          = U.unsafeSlice i (U.length xs - i) xs
          where
            (isMember, !i) = VExt.binSearchMemberIdx (mkPackedIdx x) xs

        computeScore
          :: PrimMonad m
          => (V.MVector (PrimState m) (U.Vector PackedIdx) -> StrIdx -> m (Maybe Submatch))
          -> V.MVector (PrimState m) (U.Vector PackedIdx)
          -> StrIdx
          -> m (Maybe Submatch)
        computeScore recur !needleOccursInHaystack !cutoffIndex = do
          -- Debug.Trace.traceM $ "key = " ++ show (VM.length needleOccursInHaystack, cutoffIndex)

          (remainingOccurrences :: U.Vector PackedIdx) <- bigger cutoffIndex <$> VM.unsafeRead needleOccursInHaystack 0
          case VM.length needleOccursInHaystack of
            -- Last character, already checked that vector is never empty
            1 ->
              inline findBestWith remainingOccurrences $ \ !pidx -> do
                let StrIdx !idx = getStrIdx pidx
                pure $! Just $! Submatch
                  { smScore           = heatmap `P.unsafeIndex` fromIntegral idx
                  , smPositions       = StrIdx idx :| []
                  , smContiguousCount = 0
                  }

            _ ->
              inline findBestWith remainingOccurrences $ \ !pidx -> do
                let !idx' = getStrIdx pidx
                submatch' <- recur (VM.unsafeTail needleOccursInHaystack) idx'
                pure $ (`fmap` submatch') $ \Submatch{smScore, smContiguousCount, smPositions} ->
                  let score'          = smScore + (heatmap `P.unsafeIndex` fromIntegral (unStrIdx idx'))
                      contiguousBonus = Heat $ 60 + 15 * min 3 smContiguousCount
                      isContiguous    = NE.head smPositions == succ idx'
                      score
                        | isContiguous
                        = score' + contiguousBonus
                        | otherwise
                        = score'
                  in Submatch
                    { smScore           = score
                    , smPositions       = NE.cons idx' smPositions
                    , smContiguousCount =
                      if isContiguous then smContiguousCount + 1 else 0
                    }

      !result <- do
        res <- memoizeBy makeKey computeScore occurs (StrIdx (-1))
        pure $ case res of
          Nothing -> noMatch
          Just m  -> submatchToMatch m
      pure result
  where
    noMatch = Match
      { mScore     = (-1)
      , mPositions = StrIdx (-1) :| []
      }

    makeKey :: V.MVector s (U.Vector a) -> StrIdx -> Int
    makeKey !occs !k =
      j `unsafeShiftL` 32 .|. fromIntegral (unStrIdx k)
      where
        !j = VM.length occs

    findBestWith :: forall n. Monad n => U.Vector PackedIdx -> (PackedIdx -> n (Maybe Submatch)) -> n (Maybe Submatch)
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
                  (y, Nothing)       -> y
                  (Just b', Just x') ->
                    -- If scores are equal then prefer the match occuring later.
                    Just $! if smScore x' >= smScore b' then x' else b'
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

newtype StrIdx = StrIdx { unStrIdx :: Int32 }
  deriving (Eq, Ord, Enum, Pretty)

instance Show StrIdx where
  show = show . unStrIdx

newtype instance U.MVector s StrIdx = MV_StrIdx (U.MVector s Int32)
newtype instance U.Vector    StrIdx = V_StrIdx  (U.Vector    Int32)
deriving instance GM.MVector U.MVector StrIdx
deriving instance G.Vector   U.Vector  StrIdx
instance U.Unbox StrIdx

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
  = Left $! Group { gPrevChar = firstSep, gLen = fullStrLen, gStr = fullStr, gStart = 0 }
  | otherwise
  = Right $! go fullStrLen fullStr
  where
    go :: Int -> Text -> [Group]
    go !off !str
      | T.null str
      = []
      | T.null prefix
      = if isSep $ fromIntegral $ ord $ T.unsafeHead suffix
        then
          [ Group
            { gPrevChar = T.unsafeHead suffix
            , gLen      = len - 1
            , gStr      = T.unsafeTail suffix
            , gStart    = off - len + 1
            }
          , Group
            { gPrevChar = firstSep
            , gLen      = 0
            , gStr      = T.empty
            , gStart    = 0
            }
          ]
        else
          [ Group
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
        isSep  = PExt.binSearchMember seps
        (!len, !prefix, !suffix) = T.spanLenEnd (not . isSep . fromIntegral) str

newtype Heat = Heat { unHeat :: Int32 }
  deriving (Eq, Ord, Num, Prim, Pretty, Bounded)

instance Show Heat where
  show = show . unHeat

-- | Heatmap mapping characters to scores
newtype Heatmap = Heatmap { unHeatmap :: P.Vector Heat }
  deriving (Show)

computeHeatmap :: ReusableState s -> Text -> Int -> PrimArray Int32 -> ST s Heatmap
computeHeatmap ReusableState{rsHeatmapStore} !haystack !haystackLen groupSeps = do
  vec    <- readSTRef rsHeatmapStore
  scores <- do
    let !currSize = PM.length vec
    vec' <-
      if currSize > haystackLen
      then pure vec
      else do
        vec' <- PM.unsafeNew (haystackLen * 2)
        writeSTRef rsHeatmapStore vec'
        pure vec'
    pure $ PM.unsafeSlice 0 haystackLen vec'

  let split = splitWithSeps ' ' groupSeps haystack haystackLen

      !groupsCount = case split of
        Left Group{} -> 1
        Right xs     -> length xs

  let initScore, initAdjustment :: Heat
      initScore = (-35)
      !initAdjustment = if groupsCount > 1 then fromIntegral groupsCount * (-2) else 0
      lastCharBonus :: Heat
      !lastCharBonus = 1

  PM.set scores (initScore + initAdjustment)
  update (haystackLen - 1) lastCharBonus scores

  let initGroupState = GroupState
        { gsIsBasePath = False
        , gsGroupIdx   = fi32 $ groupsCount - 1
        }
  case split of
    Left  g      -> void $ analyzeGroup g False groupsCount initGroupState scores
    Right groups -> do
      goGroups False initGroupState groups
      where
        goGroups !seenBasePath !s = \case
          []     -> pure ()
          g : gs -> do
            s' <- analyzeGroup g seenBasePath groupsCount s scores
            goGroups (seenBasePath || gsIsBasePath s') s' gs

  Heatmap <$> P.unsafeFreeze scores

data GroupState = GroupState
  { gsIsBasePath        :: !Bool
  , gsGroupIdx          :: {-# UNPACK #-} !Int32
  } deriving (Show)

data GroupChar = GroupChar
  { gcIsWord      :: !Bool
  , gcIsUpper     :: !Bool
  , gcWordCount   :: {-# UNPACK #-} !Int
  , gcWordIdx     :: {-# UNPACK #-} !Int
  , gcWordCharIdx :: {-# UNPACK #-} !Int
  , gcPrevChar    :: Int#
  } deriving (Show)

{-# INLINE fi32 #-}
fi32 :: Integral a => a -> Int32
fi32 = fromIntegral

unST :: State# s -> ST s a -> (# State# s, a #)
unST s (ST f) = f s

analyzeGroup :: Group -> Bool -> Int -> GroupState -> PM.MVector s Heat -> ST s GroupState
analyzeGroup Group{gPrevChar, gLen, gStr, gStart} !seenBasePath !groupsCount GroupState{gsGroupIdx} !scores = do
  let start :: Int
      !start = gStart
      !end = start + gLen

  let wordStart, leadingPenalty :: Heat
      !wordStart = 85
      !leadingPenalty = (-45)

  GroupChar{gcWordIdx, gcWordCharIdx, gcWordCount} <- ST $ \s -> T.textFoldIdxMIntIdx
    (\ !idx c GroupChar{gcIsWord, gcIsUpper, gcWordCount, gcWordIdx, gcWordCharIdx, gcPrevChar} s' -> unST s' $ do
      let j :: Int
          !j = start + idx

          !currWord   = isWord c
          !currUpper  = isUpper (chr (I# c))

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

      when (penaliseIfLeading gcPrevChar) $
        update j leadingPenalty scores

      pure GroupChar
        { gcIsWord      = currWord
        , gcWordCount   = if isWord' then gcWordCount + 1 else gcWordCount
        , gcIsUpper     = currUpper
        , gcWordIdx     = gcWordIdx'
        , gcWordCharIdx = gcWordCharIdx''
        , gcPrevChar    = c
        })
    (GroupChar { gcIsWord = isWordC gPrevChar, gcIsUpper = isUpper gPrevChar, gcWordCount = 0, gcWordIdx = (-1), gcWordCharIdx = 0, gcPrevChar = let !(I# x) = ord gPrevChar in x })
    gStr
    s

  when (gStart == 0 && gLen == 0) $
    update gStart wordStart scores

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

penaliseIfLeading :: Int# -> Bool
penaliseIfLeading x = isTrue# (x ==# 46#) -- 46 is '.'in ascii

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
      -- If we reached here then i == end
      | i < PM.length scores
      = update i score scores
      | otherwise
      = pure ()
