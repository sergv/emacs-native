----------------------------------------------------------------------------
-- |
-- Module      :  Data.Text.Ext
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

module Data.Text.Ext
  ( textFoldIdx
  , textFoldM
  , textFoldIdxM
  , textFoldIdxM'
  , textTraverse_
  , textFor_
  , textTraverseIdx_
  , textForIdx_
  , textToPrimArray
  , textToPrimVector
  , spanLen
  ) where

import Control.Monad.ST.Strict
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
import GHC.Word

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

