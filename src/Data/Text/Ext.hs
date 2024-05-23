----------------------------------------------------------------------------
-- |
-- Module      :  Data.Text.Ext
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE LinearTypes   #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Text.Ext
  ( textFoldLinear
  , textFoldIdx
  , textFoldM
  , textFoldIdxM
  , textCountMatches
  , textFoldIdxM'
  , textFoldIntIdxM
  , textTraverse_
  , textFor_
  , textTraverseIdx_
  , textForIdx_
  , textToPrimArray
  , textToPrimVector
  , spanLen
  , spanLenEnd
  , spanAscii2_
  ) where

import Control.Monad.ST.Strict
import Data.Int
import Data.Primitive.PrimArray
import Data.StrIdx
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Array qualified as TA
import Data.Text.Internal qualified as TI
import Data.Text.Unsafe qualified as TU
import Data.Vector.Primitive qualified as P
import Data.Vector.Primitive.Mutable qualified as PM
import Data.Word
import GHC.Exts
import GHC.Int
import GHC.Word

{-# INLINE textFoldLinear #-}
textFoldLinear :: forall (a :: UnliftedType). (Char -> a %1 -> a) -> a %1 -> Text -> a
textFoldLinear f seed (TI.Text arr off len) =
  textFoldLinearLoop seed (StrByteIdx off)
  where
    !end = StrByteIdx $ off + len
    textFoldLinearLoop :: a %1 -> StrByteIdx Int -> a
    textFoldLinearLoop x !j
      | j >= end  = x
      | otherwise =
        let TU.Iter c delta = TU.iterArray arr $ unStrByteIdx j
        in textFoldLinearLoop (inline f c x) (byteIdxAdvance j delta)

{-# INLINE textFoldIdx #-}
textFoldIdx :: forall a b. Num b => (StrCharIdx b -> Char -> a -> a) -> a -> Text -> a
textFoldIdx f !seed (TI.Text arr off len) =
  textFoldIdxLoop seed (StrCharIdx 0) (StrByteIdx off)
  where
    !end = StrByteIdx $ off + len
    textFoldIdxLoop :: a -> StrCharIdx b -> StrByteIdx Int -> a
    textFoldIdxLoop !x !i !j
      | j >= end  = x
      | otherwise =
        let TU.Iter c delta = TU.iterArray arr $ unStrByteIdx j
        in textFoldIdxLoop (inline f i c x) (charIdxAdvance i 1) (byteIdxAdvance j delta)

{-# INLINE textFoldM #-}
textFoldM :: forall m a. Monad m => (Char -> a -> m a) -> a -> Text -> m a
textFoldM f !seed (TI.Text arr off len) =
  textFoldLoop seed (StrByteIdx off)
  where
    !end = StrByteIdx $ off + len
    textFoldLoop :: a -> StrByteIdx Int -> m a
    textFoldLoop !x !j
      | j >= end  = pure x
      | otherwise = do
        let TU.Iter c delta = TU.iterArray arr $ unStrByteIdx j
        !x' <- inline f c x
        textFoldLoop x' (byteIdxAdvance j delta)

{-# INLINE textFoldIdxM #-}
textFoldIdxM
  :: forall m a b. (Monad m, Num b)
  => (StrCharIdx b -> StrByteIdx Int -> Char -> a -> m a)
  -> a
  -> Text
  -> m a
textFoldIdxM f !seed (TI.Text arr off len) =
  textFoldIdxMLoop seed (StrCharIdx 0) (StrByteIdx off)
  where
    !end = StrByteIdx $ off + len
    textFoldIdxMLoop :: a -> StrCharIdx b -> StrByteIdx Int -> m a
    textFoldIdxMLoop !x !i !j
      | j >= end  = pure x
      | otherwise = do
        let TU.Iter c delta = TU.iterArray arr $ unStrByteIdx j
        x' <- inline f i j c x
        textFoldIdxMLoop x' (charIdxAdvance i 1) (byteIdxAdvance j delta)

{-# INLINE textCountMatches #-}
textCountMatches :: (Char -> Bool) -> Text -> Int32 -> Int32
textCountMatches f (TI.Text arr off len) (I32# start) =
  countMatchesLoop start (StrByteIdx off)
  where
    !end = StrByteIdx $ off + len

    countMatchesLoop :: Int32# -> StrByteIdx Int -> Int32
    countMatchesLoop matches !j
      | j >= end  = I32# matches
      | otherwise =
        let TU.Iter c delta = TU.iterArray arr $ unStrByteIdx j
        in countMatchesLoop (if f c then matches `plusInt32#` intToInt32# 1# else matches) (byteIdxAdvance j delta)

{-# INLINE textFoldIdxM' #-}
textFoldIdxM'
  -- :: forall s (a :: TYPE ('BoxedRep 'Unlifted)).
  :: forall s (a :: TYPE 'IntRep) b.
     Num b
  => (StrCharIdx b -> Int# -> a -> State# s -> (# State# s, a #))
  -> a
  -> Text
  -> State# s
  -> (# State# s, a #)
textFoldIdxM' f seed (TI.Text arr off len) = textFoldIdxMLoop seed (StrCharIdx 0) (StrByteIdx off)
  where
    !end = StrByteIdx $ off + len
    textFoldIdxMLoop :: a -> StrCharIdx b -> StrByteIdx Int -> State# s -> (# State# s, a #)
    textFoldIdxMLoop acc !i !j s
      | j >= end  = (# s, acc #)
      | otherwise =
        case iterArray' arr j of
          (# charCode, delta #) ->
            case inline f i charCode acc s of
              (# s2, x' #) ->
                textFoldIdxMLoop x' (charIdxAdvance i 1) (byteIdxAdvance j delta) s2

{-# INLINE textFoldIntIdxM #-}
textFoldIntIdxM
  -- :: forall s (a :: TYPE ('BoxedRep 'Unlifted)).
  :: forall s a b.
      Num b
  => (StrCharIdx b -> Int# -> a -> State# s -> (# State# s, a #))
  -> a
  -> StrCharIdx b
  -> Text
  -> State# s
  -> (# State# s, a #)
textFoldIntIdxM f seed start (TI.Text arr off len) =
  textFoldIntIdxMLoop seed start (StrByteIdx off)
  where
    !end = StrByteIdx $ off + len
    textFoldIntIdxMLoop :: a -> StrCharIdx b -> StrByteIdx Int -> State# s -> (# State# s, a #)
    textFoldIntIdxMLoop !acc !i !j s
      | j >= end  = (# s, acc #)
      | otherwise =
        case iterArray' arr j of
          (# charCode, !delta #) ->
            case inline f i charCode acc s of
              (# s2, !x' #) ->
                textFoldIntIdxMLoop x' (charIdxAdvance i 1) (byteIdxAdvance j delta) s2

reverseIterArray' :: TA.Array -> Int -> (# Int#, Int #)
reverseIterArray' arr j =
  if isTrue# (m0# `ltWord#` 0x80##)
  then (# word2Int# m0#, (-1) #)
  else
    let !m1 = TA.unsafeIndex arr (j - 1) in
    if m1 >= 0xC0
    then (# wchr2 m1 m0, (-2) #)
    else
      let !m2 = TA.unsafeIndex arr (j - 2) in
      if m2 >= 0xC0
      then (# wchr3 m2 m1 m0, (-3) #)
      else
        let !m3 = TA.unsafeIndex arr (j - 3) in
        (# wchr4 m3 m2 m1 m0, (-4) #)
  where
    !m0@(W8# m0_8#) = TA.unsafeIndex arr j
    m0# = word8ToWord# m0_8#

{-# INLINE iterArray' #-}
iterArray' :: TA.Array -> StrByteIdx Int -> (# Int#, Int #)
iterArray' arr j = (# w, l #)
  where
    m0 :: Word8
    !m0 = TA.unsafeIndex arr $ unStrByteIdx j

    l :: Int
    !l  = utf8LengthByLeader m0
    !w  = case l of
      1 -> case m0 of W8# m0_8# -> word2Int# (word8ToWord# m0_8#)
      2 -> wchr2 m0
        (TA.unsafeIndex arr (unStrByteIdx (byteIdxAdvance j 1)))
      3 -> wchr3 m0
        (TA.unsafeIndex arr (unStrByteIdx (byteIdxAdvance j 1)))
        (TA.unsafeIndex arr (unStrByteIdx (byteIdxAdvance j 2)))
      _ -> wchr4 m0
        (TA.unsafeIndex arr (unStrByteIdx (byteIdxAdvance j 1)))
        (TA.unsafeIndex arr (unStrByteIdx (byteIdxAdvance j 2)))
        (TA.unsafeIndex arr (unStrByteIdx (byteIdxAdvance j 3)))

{-# INLINE utf8LengthByLeader #-}
utf8LengthByLeader :: Word8 -> Int
utf8LengthByLeader (W8# w) = I# (c# `xorI#` (c# <=# 0#))
  where
    c# = word2Int# (clz8# (word8ToWord# (notWord8# w)))

{-# INLINE wchr2 #-}
wchr2 :: Word8 -> Word8 -> Int#
wchr2 (W8# x1#) (W8# x2#) =
  z1# +# z2#
  where
    !y1# = word2Int# (word8ToWord# x1#)
    !y2# = word2Int# (word8ToWord# x2#)
    !z1# = uncheckedIShiftL# (y1# -# 0xC0#) 6#
    !z2# = y2# -# 0x80#

{-# INLINE wchr3 #-}
wchr3 :: Word8 -> Word8 -> Word8 -> Int#
wchr3 (W8# x1#) (W8# x2#) (W8# x3#) =
  z1# +# z2# +# z3#
  where
    !y1# = word2Int# (word8ToWord# x1#)
    !y2# = word2Int# (word8ToWord# x2#)
    !y3# = word2Int# (word8ToWord# x3#)
    !z1# = uncheckedIShiftL# (y1# -# 0xE0#) 12#
    !z2# = uncheckedIShiftL# (y2# -# 0x80#) 6#
    !z3# = y3# -# 0x80#

{-# INLINE wchr4 #-}
wchr4 :: Word8 -> Word8 -> Word8 -> Word8 -> Int#
wchr4 (W8# x1#) (W8# x2#) (W8# x3#) (W8# x4#) =
  z1# +# z2# +# z3# +# z4#
  where
    !y1# = word2Int# (word8ToWord# x1#)
    !y2# = word2Int# (word8ToWord# x2#)
    !y3# = word2Int# (word8ToWord# x3#)
    !y4# = word2Int# (word8ToWord# x4#)
    !z1# = uncheckedIShiftL# (y1# -# 0xF0#) 18#
    !z2# = uncheckedIShiftL# (y2# -# 0x80#) 12#
    !z3# = uncheckedIShiftL# (y3# -# 0x80#) 6#
    !z4# = y4# -# 0x80#

{-# INLINE textFor_ #-}
textFor_ :: forall m. Monad m => Text -> (Char -> m ()) -> m ()
textFor_ = flip textTraverse_

{-# INLINE textTraverse_ #-}
textTraverse_ :: forall m. Monad m => (Char -> m ()) -> Text -> m ()
textTraverse_ f (TI.Text arr off len) =
  textTraverseLoop (StrByteIdx off)
  where
    !end = StrByteIdx $ off + len
    textTraverseLoop :: StrByteIdx Int -> m ()
    textTraverseLoop !j
      | j >= end  = pure ()
      | otherwise = do
        let TU.Iter c delta = TU.iterArray arr $ unStrByteIdx j
        f c
        textTraverseLoop (byteIdxAdvance j delta)

{-# INLINE textForIdx_ #-}
textForIdx_ :: forall m b. (Monad m, Num b) => Text -> (StrCharIdx b -> Char -> m ()) -> m ()
textForIdx_ = flip textTraverseIdx_

{-# INLINE textTraverseIdx_ #-}
textTraverseIdx_ :: forall m b. (Monad m, Num b) => (StrCharIdx b -> Char -> m ()) -> Text -> m ()
textTraverseIdx_ f (TI.Text arr off len) =
  textTraverseIdxLoop (StrCharIdx 0) (StrByteIdx off)
  where
    !end = StrByteIdx $ off + len
    textTraverseIdxLoop :: StrCharIdx b -> StrByteIdx Int -> m ()
    textTraverseIdxLoop !i !j
      | j >= end  = pure ()
      | otherwise = do
        let TU.Iter c delta = TU.iterArray arr $ unStrByteIdx j
        f i c
        textTraverseIdxLoop (charIdxAdvance i 1) (byteIdxAdvance j delta)

textToPrimArray :: Text -> PrimArray Char
textToPrimArray str = runST $ do
  arr <- newPrimArray (T.length str)
  _   <- textTraverseIdx_ (writePrimArray arr . unStrCharIdx) str
  unsafeFreezePrimArray arr

textToPrimVector :: Text -> P.Vector Char
textToPrimVector str = runST $ do
  arr <- PM.unsafeNew (T.length str)
  _   <- textTraverseIdx_ (PM.unsafeWrite arr . unStrCharIdx) str
  P.unsafeFreeze arr

{-# INLINE spanLen #-}
spanLen :: (Int -> Bool) -> Text -> (Int, Text, Text)
spanLen p (TI.Text arr off len) = (charCount', hd, tl)
  where
    hd = TI.text arr off (k - off)
    tl = TI.text arr k (len + off - k)
    charCount' :: Int
    (# !charCount', StrByteIdx !k #) = loop 0 $ StrByteIdx off

    !end = StrByteIdx $ off + len

    loop :: Int -> StrByteIdx Int -> (# Int, StrByteIdx Int #)
    loop !charCount !i
      | i < end && p (I# c) = loop (charCount + 1) (byteIdxAdvance i d)
      | otherwise           = (# charCount, i #)
      where
        !(# c, d #) = iterArray' arr i

{-# INLINE spanLenEnd #-}
spanLenEnd :: (Int -> Bool) -> Text -> (Int, Text, Text)
spanLenEnd p (TI.Text arr start len) = (charCount', hd, tl)
  where
    hd = TI.text arr start (k - start)
    tl = TI.text arr k (len + start - k)

    charCount' :: Int
    (# !charCount', !k #) = loop 0 end
    !end = start + len - 1
    loop :: Int -> Int -> (# Int, Int #)
    loop !charCount !i
      | i > start
      = let !(# c, d #) = reverseIterArray' arr i in
        if p (I# c)
        then loop (charCount + 1) (i + d)
        else (# charCount, i #)
      | otherwise
      = (# charCount + 1, i #)

{-# INLINE spanAscii2_ #-}
spanAscii2_ :: (Word8 -> Bool) -> (Word8 -> Bool) -> Text -> (# Text, Text #)
spanAscii2_ p1 p2 (TI.Text arr off len) = (# hd, tl #)
  where
    hd = TI.text arr off k
    tl = TI.text arr (off + k) (len - k)
    !k = loop1 0
    loop1 !i
      | i < len && p1 (TA.unsafeIndex arr (off + i))
      = loop1 (i + 1)
      | otherwise
      = loop2 i
    loop2 !i
      | i < len && p2 (TA.unsafeIndex arr (off + i))
      = loop2 (i + 1)
      | otherwise
      = i
