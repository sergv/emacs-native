----------------------------------------------------------------------------
-- |
-- Module      :  Control.LensBlaze.Tests
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.LensBlaze.Tests (tests) where

import Data.Coerce
import Data.Int
import Data.Proxy
import Data.Word
import GHC.TypeNats
import Numeric
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC

import Control.LensBlaze

tests :: TestTree
tests = testGroup "Control.LensBlaze.Tests"
  [ localOption (QC.QuickCheckTests 10000) $
    QC.testProperty "Int21" $
      \(carrier :: Word64) (x :: Int21) (Offset off :: Offset 0 43) ->
        (view (int21L off) (set (int21L off) (abs (unInt21 x)) carrier)) === abs (unInt21 x)
  , localOption (QC.QuickCheckTests 10000) $
    QC.testProperty "Word21" $
      \(carrier :: Word64) (x :: Word21) (Offset off :: Offset 0 43) ->
        view (word21L off) (set (word21L off) (unWord21 x) carrier) === unWord21 x
  ]

newtype Int21 = Int21 { unInt21 :: Int32 }
  deriving (Eq, Ord)

instance Show Int21 where
  showsPrec _ = (showString "0x" .) . showHex . (fromIntegral :: Int32 -> Word32) . unInt21

instance Bounded Int21 where
  minBound = Int21 (-(2 ^ 21 - 1))
  maxBound = Int21 (2 ^ 21 - 1)

newtype Word21 = Word21 { unWord21 :: Word32 }
  deriving (Eq, Ord)

instance Show Word21 where
  showsPrec _ = (showString "0x" .) . showHex . unWord21

instance Bounded Word21 where
  minBound = Word21 0
  maxBound = Word21 (2 ^ 21 - 1)

instance Arbitrary Int21 where
  arbitrary = do
    Int21 . fromIntegral <$> chooseInt64 (fromIntegral (unInt21 minBound), fromIntegral (unInt21 maxBound))
  shrink (Int21 x) =
    filter (\y -> minBound <= y && y <= maxBound) $ coerce $ shrink x

instance Arbitrary Word21 where
  arbitrary = do
    Word21 . fromIntegral <$> chooseWord64 (fromIntegral (unWord21 minBound), fromIntegral (unWord21 maxBound))
  shrink (Word21 x) =
    filter (\y -> minBound <= y && y <= maxBound) $ coerce $ shrink x

newtype Offset (from :: Nat) (to :: Nat) = Offset { unOffset :: Int }
  deriving (Eq, Ord, Show)

instance (KnownNat from, KnownNat to, from <= to) => Bounded (Offset from to) where
  minBound = Offset $ fromIntegral $ natVal (Proxy @from)
  maxBound = Offset $ fromIntegral $ natVal (Proxy @to)

instance (KnownNat from, KnownNat to, from <= to) => Arbitrary (Offset from to) where
  arbitrary = do
    Offset . fromIntegral <$> chooseWord64 (fromIntegral (unOffset (minBound :: Offset from to)), fromIntegral (unOffset (maxBound :: Offset from to)))
  shrink (Offset x) =
    filter (\y -> minBound <= y && y <= maxBound) $ coerce $ shrink x


