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
import GHC.Generics (Generic)
import GHC.Int (Int32(I32#))
import GHC.Magic (inline)
import GHC.ST
import GHC.Types
import GHC.Word (Word64(W64#))
import Numeric (showHex)
import Prettyprinter
import Prettyprinter.Show

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
  { rsHaystackStore :: !(STRef s (MutableByteArray s))
  , rsHeatmapStore  :: !(STRef s (MutablePrimArray s Heat))
  , rsNeedleStore   :: !(VM.MVector s (U.Vector PackedIdx))
  }

mkReusableState :: Int -> NeedleChars -> ST s (ReusableState s)
mkReusableState !needleSize !chars = do
  rsHaystackStore <- newSTRef =<< newByteArray (needleCharsCountHint chars)
  rsHeatmapStore  <- newSTRef =<< newPrimArray (needleCharsCountHint chars)
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
              if toBool (isUpperASCII charCode)
              then
                case writeByteArray# mbarr (j +# 1#) (combineCharIdx (fromIntegral (I# (toLowerASCII charCode))) i) s2 of
                  s3 -> (# s3, j +# 2# #)
              else (# s2, j +# 1# #)
        | otherwise =
          (# s1, j #)
        where
          !c' = wordToWord64# (int2Word# charCode)

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
                  let !c = chr (I# charCode) in
                  if isUpper c
                  then
                    case writeByteArray# mbarr (j +# 1#) (combineCharIdx (fromIntegral (ord (toLower c))) i) s2 of
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

isUpperASCII :: Int# -> Bool#
isUpperASCII x = Bool# ((64# <# x) `andI#` (x <# 91#))

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
  sortWord64 haystackMut
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
                  { smScore           = heatmap `indexPrimArray` fromIntegral idx
                  , smPositions       = StrIdx idx :| []
                  , smContiguousCount = 0
                  }

            _ ->
              inline findBestWith remainingOccurrences $ \ !pidx -> do
                let !idx' = getStrIdx pidx
                submatch' <- recur (VM.unsafeTail needleOccursInHaystack) idx'
                pure $ (`fmap` submatch') $ \Submatch{smScore, smContiguousCount, smPositions} ->
                  let score'          = smScore + (heatmap `indexPrimArray` fromIntegral (unStrIdx idx'))
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
      { mScore     = (-1000000)
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

{-# INLINE splitWithSeps #-}
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
newtype Heatmap = Heatmap { unHeatmap :: PrimArray Heat }
  deriving (Show)

-- Heatmap elements spast @hastyackLen@ will have undefined values. Take care not
-- to access them!
computeHeatmap :: ReusableState s -> Text -> Int -> PrimArray Int32 -> ST s Heatmap
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
      !lastCharBonus = 1

  setPrimArray scores 0 haystackLen (Heat (fi32 (initScore + initAdjustment)))
  update (haystackLen - 1) lastCharBonus scores

  let initGroupState = GroupState
        { gsIsBasePath = False
        , gsGroupIdx   = groupsCount - 1
        }

  case split of
    Left  g      -> void $ analyzeGroup g False groupsCount initGroupState haystackLen scores
    Right groups -> goGroups False initGroupState groups
      where
        goGroups !seenBasePath !s = \case
          []     -> pure ()
          g : gs -> do
            s' <- analyzeGroup g seenBasePath groupsCount s haystackLen scores
            goGroups (seenBasePath || gsIsBasePath s') s' gs

  Heatmap <$> unsafeFreezePrimArray scores

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
  let start :: Int
      !start = gStart
      !end = start + gLen

  let wordStart :: Heat
      !wordStart = 85

  let wordStart#, leadingPenalty# :: Int#
      wordStart#      = 85#
      leadingPenalty# = (-45#)

  GroupChar{gcWordIdx, gcWordCharIdx, gcWordCount} <- ST $ \s -> T.textFoldIntIdxM
    (\ (!idx :: Int) (c :: Int#) GroupChar{gcIsWord, gcIsUpper, gcWordCount, gcWordIdx, gcWordCharIdx, gcPrevChar} s' -> unST s' $ do

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

  when (gStart == 0 && gLen == 0) $
    update gStart wordStart scores

  -- Update score for trailing separator of current group.
  let !trailingSep = end
  when (trailingSep < scoresLen && gcWordIdx >= 0) $
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
  | otherwise  = if gcGroupIdx == 0 then (- 3) else Heat $ fi32 $ gcGroupIdx - 6

penaliseIfLeading :: Int# -> Bool#
penaliseIfLeading x = Bool# (x <=# 46#) `band#` Bool# (x >=# 46#) -- 46 is '.' in ascii

update :: Int -> Heat -> MutablePrimArray s Heat -> ST s ()
update !idx !val !arr = do
  x <- readPrimArray arr idx
  writePrimArray arr idx (x + val)

applyGroupScore :: Heat -> Int -> Int -> Int -> MutablePrimArray s Heat -> ST s ()
applyGroupScore !score !start !end !arrLen !scores =
  go start
  where
    go !i
      | i < end
      = update i score scores *> go (i + 1)
      -- If we reached here then i == end
      | i < arrLen
      = update i score scores
      | otherwise
      = pure ()
