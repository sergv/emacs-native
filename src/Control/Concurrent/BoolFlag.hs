-- |
-- Module:     Control.Concurrent.BoolFlag
-- Copyright:  (c) Sergey Vinokurov 2026
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Control.Concurrent.BoolFlag
  ( BoolFlag
  , new
  , get
  , or
  ) where

import Prelude hiding (or)

import Control.Concurrent.Counter qualified as Counter
import Data.Functor (void)

newtype BoolFlag = BoolFlag { unBoolFlag :: Counter.Counter }

boolToInt :: Bool -> Int
boolToInt = \case
  False -> 0
  True  -> 1

intToBool :: Int -> Bool
intToBool = \case
  1 -> True
  _ -> False

new :: Bool -> IO BoolFlag
new = fmap BoolFlag . Counter.new . boolToInt

get :: BoolFlag -> IO Bool
get = fmap intToBool . Counter.get . unBoolFlag

or :: BoolFlag -> Bool -> IO ()
or (BoolFlag counter) = void . Counter.or counter . boolToInt
