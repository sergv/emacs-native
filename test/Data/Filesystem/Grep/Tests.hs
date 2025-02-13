-- |
-- Module:     Data.Filesystem.Grep.Tests
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Filesystem.Grep.Tests (tests) where

import Control.Monad
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Interleave
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Primitive qualified as VP
import Data.Vector.Unboxed qualified as VU
import Prettyprinter.Combinators
import System.OsPath
import Test.Tasty
import Test.Tasty.HUnit

import Data.Emacs.Module.Env.ProcessInput (Result(Continue))
import Data.Filesystem.Find
import Data.Filesystem.Grep
import Data.Ignores
import Emacs.Module.Monad.Class

tests :: TestTree
tests = testGroup "Data.Filesystem.Grep.Tests"
  [ testCase "grep 1" $ do
      let path = [osp|test|] </> [osp|Data|] </> [osp|Filesystem|] </> [osp|Grep|] </> [osp|Tests.hs|]
          expected = MatchEntry
            { matchAbsPath    = AbsFile $ [osp|.|] </> path
            , matchRelPath    = RelFile path
            , matchLineNum    = 12
            , matchColumnNum  = 0
            , matchLinePrefix = mempty
            , matchLineStr    = "module Data.Filesystem.Grep.Tests"
            , matchLineSuffix = " (tests) where"
            }
      xs  <- grep' [osp|.|] "^module Data.Filesystem.Grep.Tests" ["*.hs"] False
      checkEqual xs [expected]
  , testCase "grep unicode 1" $ do
      let path = [osp|test-data|] </> [osp|test.txt|]
          expected = MatchEntry
            { matchAbsPath    = AbsFile $ [osp|.|] </> path
            , matchRelPath    = RelFile path
            , matchLineNum    = 2
            , matchColumnNum  = 16
            , matchLinePrefix = T.encodeUtf8 "〚decombobulate"
            , matchLineStr    = T.encodeUtf8 "〛"
            , matchLineSuffix = ""
            }
      xs  <- grep' [osp|.|] "〛" ["*.txt"] False
      checkEqual xs [expected]
  ]

checkEqual
  :: (Eq a, Show a, Pretty a)
  => a      -- ^ The expected value
  -> a      -- ^ The actual value
  -> Assertion
checkEqual actual expected = unless (actual == expected) $ assertFailure msg
  where
    msg = T.unpack $ render $ ppDictHeader "Different results"
      [ "actual"   --> actual
      , "expected" --> expected
      ]

grep' :: OsPath -> Text -> [Text] -> Bool -> IO [MatchEntry]
grep' root reToFind globs ignoreCase = runDummyEmacsM $
  toList <$> grep [root] reToFind globs ignoreCase dummyIgnores dummyIgnores (\_ entry -> pure entry)

newtype DummyEmacsM s a = DummyEmacsM { runDummyEmacsM :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadBaseControl IO, MonadThrow, MonadInterleave, VGM.PrimMonad)

newtype DummyValue s = DummyValue Int
  deriving (VU.Unbox)

newtype instance VU.MVector s (DummyValue _) = MV_DummyValue (VP.MVector s Int)
newtype instance VU.Vector    (DummyValue _) = V_DummyValue  (VP.Vector    Int)
deriving via (VU.UnboxViaPrim Int) instance VGM.MVector VU.MVector (DummyValue s)
deriving via (VU.UnboxViaPrim Int) instance VG.Vector   VU.Vector  (DummyValue s)

instance MonadEmacs DummyEmacsM DummyValue where
  makeGlobalRef             = error "Not implemented"
  freeGlobalRef             = error "Not implemented"
  nonLocalExitCheck         = error "Not implemented"
  nonLocalExitGet           = error "Not implemented"
  nonLocalExitSignal        = error "Not implemented"
  nonLocalExitThrow         = error "Not implemented"
  nonLocalExitClear         = error "Not implemented"
  makeFunction _ _          = error "Not implemented"
  funcall                   = error "Not implemented"
  funcallPrimitive          = error "Not implemented"
  funcallPrimitiveUnchecked = error "Not implemented"
  intern                    = error "Not implemented"
  typeOf                    = error "Not implemented"
  isNotNil                  = error "Not implemented"
  eq                        = error "Not implemented"
  extractWideInteger        = error "Not implemented"
  makeWideInteger           = error "Not implemented"
  extractDouble             = error "Not implemented"
  makeDouble                = error "Not implemented"
  extractText               = error "Not implemented"
  extractShortByteString    = error "Not implemented"
  makeString                = error "Not implemented"
  makeBinaryString          = error "Not implemented"
  extractUserPtr            = error "Not implemented"
  makeUserPtr               = error "Not implemented"
  assignUserPtr             = error "Not implemented"
  extractUserPtrFinaliser   = error "Not implemented"
  assignUserPtrFinaliser    = error "Not implemented"
  vecGet                    = error "Not implemented"
  unsafeVecGet              = error "Not implemented"
  vecSet                    = error "Not implemented"
  vecSize                   = error "Not implemented"

  processInput = pure Continue
