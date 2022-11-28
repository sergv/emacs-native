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
  ( textFoldM
  , textFoldIdxM
  , textFoldIdxM'
  , textTraverse_
  , textFor_
  , textTraverseIdx_
  , textForIdx_
  , textToPrimArray
  , textToPrimVector
  ) where

import Control.Monad.ST.Strict
import Data.Primitive.PrimArray
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Internal qualified as TI
import Data.Text.Unsafe qualified as TU
import Data.Vector.Primitive qualified as P
import Data.Vector.Primitive.Mutable qualified as PM
import GHC.Exts

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
textFoldIdxM f !seed (TI.Text arr off len) = textFoldIdxLoop seed 0 off
  where
    !end = off + len
    textFoldIdxLoop :: a -> Int -> Int -> m a
    textFoldIdxLoop !x !i !j
      | j >= end  = pure x
      | otherwise = do
        let TU.Iter c delta = TU.iterArray arr j
        x' <- f i c x
        textFoldIdxLoop x' (i + 1) (j + delta)

{-# INLINE textFoldIdxM' #-}
textFoldIdxM'
  -- :: forall s (a :: TYPE ('BoxedRep 'Unlifted)).
  :: forall s (a :: TYPE ('TupleRep '[ 'IntRep, UnliftedRep ])).
      (Int -> Char -> a -> State# s -> (# State# s, a #))
   -> a
   -> Text
   -> State# s
   -> (# State# s, a #)
textFoldIdxM' f seed (TI.Text arr off len) = textFoldIdxLoop seed 0 off
  where
    !end = off + len
    textFoldIdxLoop :: a -> Int -> Int -> State# s -> (# State# s, a #)
    textFoldIdxLoop x !i !j s
      | j >= end  = (# s, x #)
      | otherwise =
        case TU.iterArray arr j of
          TU.Iter c delta ->
            case f i c x s of
              (# s2, x' #) ->
                textFoldIdxLoop x' (i + 1) (j + delta) s2

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
