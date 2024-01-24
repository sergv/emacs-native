----------------------------------------------------------------------------
-- |
-- Module      :  Control.LensBlaze
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Control.LensBlaze
  ( Lens
  , Lens'
  , lens
  , view
  , over
  , set
  , coerceL
  , boolL
  , int16L
  , int21L
  , word21L
  , int32L
  , word32L
  , integralL
  ) where

import Control.Applicative

import Data.Bits
import Data.Coerce
import Data.Functor.Identity
import Data.Int
import Data.Word

type Lens' s a = Lens s s a a
type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

{-# INLINE lens #-}
lens :: (s -> a) -> (b -> s -> t) -> Lens s t a b
lens access write = \f s -> (\b -> write b s) <$> f (access s)

{-# INLINE view #-}
view :: Lens s t a b -> s -> a
view l = getConst . l Const

{-# INLINE set #-}
set :: Lens s t a b -> b -> s -> t
set l = over l . const

{-# INLINE over #-}
over :: Lens s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

{-# INLINE coerceL #-}
coerceL :: forall s t a b. (Coercible s a, Coercible t b) => Lens s t a b
coerceL = \f x -> bt <$> f (sa x)
  where
    sa :: s -> a
    sa = coerce
    bt :: b -> t
    bt = coerce

{-# INLINE boolL #-}
boolL :: Bits b => Int -> Lens' b Bool
boolL !offset = \f x ->
  (\b' -> if b' then setBit x offset else clearBit x offset) <$> f (testBit x offset)

{-# INLINE int16L #-}
int16L :: (Bits b, Integral b) => Int -> Lens' b Int16
int16L !offset = integralL offset 0xffff

{-# INLINE int21L #-}
int21L :: Int -> Lens' Word64 Int32
int21L !offset = integralL offset 0x001fffff

{-# INLINE word21L #-}
word21L :: Int -> Lens' Word64 Word32
word21L !offset = integralL offset 0x001fffff

{-# INLINE int32L #-}
int32L :: (Bits b, Integral b) => Int -> Lens' b Int32
int32L !offset = integralL offset 0xffffffff

{-# INLINE word32L #-}
word32L :: (Bits b, Integral b) => Int -> Lens' b Word32
word32L !offset = integralL offset 0xffffffff

{-# INLINE integralL #-}
integralL :: forall a b. (Integral a, Bits b, Integral b) => Int -> b -> Lens' b a
integralL !offset !mask = \(f :: a -> f a) (x :: b) ->
  (\x' -> (fromIntegral x' `unsafeShiftL` offset) .|. (x .&. reverseMask)) <$>
    f (fromIntegral ((x `unsafeShiftR` offset) .&. mask :: b))
  where
    reverseMask :: b
    !reverseMask = complement $ mask `unsafeShiftL` offset
