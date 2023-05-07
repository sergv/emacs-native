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
  ) where

import Control.Monad.ST.Strict
import Data.Int
import Data.Primitive.PrimArray
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
textFoldLinear f seed (TI.Text arr off len) = textFoldLinearLoop seed 0 off
  where
    !end = off + len
    textFoldLinearLoop :: a %1 -> Int -> Int -> a
    textFoldLinearLoop x !i !j
      | j >= end  = x
      | otherwise =
        let TU.Iter c delta = TU.iterArray arr j
        in textFoldLinearLoop (f c x) (i + 1) (j + delta)

{-# INLINE textFoldIdx #-}
textFoldIdx :: forall a. (Int -> Char -> a -> a) -> a -> Text -> a
textFoldIdx f !seed (TI.Text arr off len) = textFoldIdxLoop seed 0 off
  where
    !end = off + len
    textFoldIdxLoop :: a -> Int -> Int -> a
    textFoldIdxLoop !x !i !j
      | j >= end  = x
      | otherwise =
        let TU.Iter c delta = TU.iterArray arr j
        in textFoldIdxLoop (f i c x) (i + 1) (j + delta)

{-# INLINE textFoldM #-}
textFoldM :: forall m a. Monad m => (Char -> a -> m a) -> a -> Text -> m a
textFoldM f !seed (TI.Text arr off len) = textFoldLoop seed off
  where
    !end = off + len
    textFoldLoop :: a -> Int -> m a
    textFoldLoop !x !j
      | j >= end  = pure x
      | otherwise = do
        let TU.Iter c delta = TU.iterArray arr j
        !x' <- f c x
        textFoldLoop x' (j + delta)

{-# INLINE textFoldIdxM #-}
textFoldIdxM :: forall m a. Monad m => (Int -> Char -> a -> m a) -> a -> Text -> m a
textFoldIdxM f !seed (TI.Text arr off len) = textFoldIdxMLoop seed 0 off
  where
    !end = off + len
    textFoldIdxMLoop :: a -> Int -> Int -> m a
    textFoldIdxMLoop !x !i !j
      | j >= end  = pure x
      | otherwise = do
        let TU.Iter c delta = TU.iterArray arr j
        x' <- f i c x
        textFoldIdxMLoop x' (i + 1) (j + delta)

{-# INLINE textCountMatches #-}
textCountMatches :: (Char -> Bool) -> Text -> Int32 -> Int32
textCountMatches f (TI.Text arr off len) (I32# start) = countMatchesLoop start off
  where
    !end = off + len

    countMatchesLoop :: Int32# -> Int -> Int32
    countMatchesLoop matches !j
      | j >= end  = I32# matches
      | otherwise =
        let TU.Iter c delta = TU.iterArray arr j
        in countMatchesLoop (if f c then matches `plusInt32#` intToInt32# 1# else matches) (j + delta)

{-# INLINE textFoldIdxM' #-}
textFoldIdxM'
  -- :: forall s (a :: TYPE ('BoxedRep 'Unlifted)).
  :: forall s (a :: TYPE 'IntRep).
      (Word64 -> Int# -> a -> State# s -> (# State# s, a #))
   -> a
   -> Text
   -> State# s
   -> (# State# s, a #)
textFoldIdxM' f seed (TI.Text arr off len) = textFoldIdxMLoop seed 0 off
  where
    !end = off + len
    textFoldIdxMLoop :: a -> Word64 -> Int -> State# s -> (# State# s, a #)
    textFoldIdxMLoop acc !i !j s
      | j >= end  = (# s, acc #)
      | otherwise =
        case iterArray' arr j of
          (# charCode, delta #) ->
            case inline f i charCode acc s of
              (# s2, x' #) ->
                textFoldIdxMLoop x' (i + 1) (j + delta) s2

{-# INLINE textFoldIntIdxM #-}
textFoldIntIdxM
  -- :: forall s (a :: TYPE ('BoxedRep 'Unlifted)).
  :: forall s a.
      (Int -> Int# -> a -> State# s -> (# State# s, a #))
   -> a
   -> Int
   -> Text
   -> State# s
   -> (# State# s, a #)
textFoldIntIdxM f seed start (TI.Text arr off len) = textFoldIntIdxMLoop seed start off
  where
    !end = off + len
    textFoldIntIdxMLoop :: a -> Int -> Int -> State# s -> (# State# s, a #)
    textFoldIntIdxMLoop !acc !i !j s
      | j >= end  = (# s, acc #)
      | otherwise =
        case iterArray' arr j of
          (# charCode, !delta #) ->
            case inline f i charCode acc s of
              (# s2, !x' #) ->
                textFoldIntIdxMLoop x' (i + 1) (j + delta) s2

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

iterArray' :: TA.Array -> Int -> (# Int#, Int #)
iterArray' arr j = (# w, l #)
  where
    !m0@(W8# m0_8#) = TA.unsafeIndex arr j

    m0# = word8ToWord# m0_8#

    !l  = utf8LengthByLeader m0#
    !w  = case l of
      1 -> word2Int# m0#
      2 -> wchr2 m0 (TA.unsafeIndex arr (j + 1))
      3 -> wchr3 m0 (TA.unsafeIndex arr (j + 1)) (TA.unsafeIndex arr (j + 2))
      _ -> wchr4 m0 (TA.unsafeIndex arr (j + 1)) (TA.unsafeIndex arr (j + 2)) (TA.unsafeIndex arr (j + 3))
{-# INLINE iterArray' #-}

utf8LengthByLeader :: Word# -> Int
utf8LengthByLeader w = I# (c# `xorI#` c# <=# 0#)
  where
    c# = word2Int# (clz# (not# w))

wchr2 :: Word8 -> Word8 -> Int#
wchr2 (W8# x1#) (W8# x2#) =
  z1# +# z2#
  where
    !y1# = word2Int# (word8ToWord# x1#)
    !y2# = word2Int# (word8ToWord# x2#)
    !z1# = uncheckedIShiftL# (y1# -# 0xC0#) 6#
    !z2# = y2# -# 0x80#
{-# INLINE wchr2 #-}

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
{-# INLINE wchr3 #-}

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
{-# INLINE wchr4 #-}

{-# INLINE textFor_ #-}
textFor_ :: forall m. Monad m => Text -> (Char -> m ()) -> m ()
textFor_ = flip textTraverse_

{-# INLINE textTraverse_ #-}
textTraverse_ :: forall m. Monad m => (Char -> m ()) -> Text -> m ()
textTraverse_ f (TI.Text arr off len) = textTraverseLoop off
  where
    !end = off + len
    textTraverseLoop :: Int -> m ()
    textTraverseLoop !j
      | j >= end  = pure ()
      | otherwise = do
        let TU.Iter c delta = TU.iterArray arr j
        f c
        textTraverseLoop (j + delta)

{-# INLINE textForIdx_ #-}
textForIdx_ :: forall m. Monad m => Text -> (Int -> Char -> m ()) -> m ()
textForIdx_ = flip textTraverseIdx_

{-# INLINE textTraverseIdx_ #-}
textTraverseIdx_ :: forall m. Monad m => (Int -> Char -> m ()) -> Text -> m ()
textTraverseIdx_ f (TI.Text arr off len) = textTraverseIdxLoop 0 off
  where
    !end = off + len
    textTraverseIdxLoop :: Int -> Int -> m ()
    textTraverseIdxLoop !i !j
      | j >= end  = pure ()
      | otherwise = do
        let TU.Iter c delta = TU.iterArray arr j
        f i c
        textTraverseIdxLoop (i + 1) (j + delta)

textToPrimArray :: Text -> PrimArray Char
textToPrimArray str = runST $ do
  arr <- newPrimArray (T.length str)
  _   <- textTraverseIdx_ (writePrimArray arr) str
  unsafeFreezePrimArray arr

textToPrimVector :: Text -> P.Vector Char
textToPrimVector str = runST $ do
  arr <- PM.unsafeNew (T.length str)
  _   <- textTraverseIdx_ (PM.unsafeWrite arr) str
  P.unsafeFreeze arr

{-# INLINE spanLen #-}
spanLen :: (Int -> Bool) -> Text -> (Int, Text, Text)
spanLen p (TI.Text arr off len) = (charCount', hd, tl)
  where
    hd = TI.text arr off (k - off)
    tl = TI.text arr k (len + off - k)
    charCount' :: Int
    (!charCount', !k) = loop 0 off
    !end = off + len
    loop !charCount !i
      | i < end && p (I# c) = loop (charCount + 1) (i + d)
      | otherwise           = (charCount, i)
      where
        !(# c, d #) = iterArray' arr i

{-# INLINE spanLenEnd #-}
spanLenEnd :: (Int -> Bool) -> Text -> (Int, Text, Text)
spanLenEnd p (TI.Text arr start len) = (charCount', hd, tl)
  where
    hd = TI.text arr start (k - start)
    tl = TI.text arr k (len + start - k)

    charCount' :: Int
    (!charCount', !k) = loop 0 end
    !end = start + len - 1
    loop !charCount !i
      | i > start
      = let !(# c, d #) = reverseIterArray' arr i in
        if p (I# c)
        then loop (charCount + 1) (i + d)
        else (charCount, i)
      | otherwise
      = (charCount + 1, i)

