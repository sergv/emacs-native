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
import Control.Monad.NoEarlyTermination
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Prettyprinter.Combinators
import System.OsPath
import Test.Tasty
import Test.Tasty.HUnit

import Data.Filesystem.Find
import Data.Filesystem.Grep
import Data.Ignores

tests :: TestTree
tests = testGroup "Data.Filesystem.Grep.Tests"
  [ testCase "grep 1" $ do
      let path     = [osp|test|] </> [osp|Data|] </> [osp|Filesystem|] </> [osp|Grep|] </> [osp|Tests.hs|]
          expected = MatchEntry
            { matchAbsPath    = AbsFile $ [osp|.|] </> path
            , matchRelPath    = RelFile path
            , matchLineNum    = 12
            , matchColumnNum  = 0
            , matchLinePrefix = T.encodeUtf8 mempty
            , matchLineStr    = T.encodeUtf8 "module Data.Filesystem.Grep.Tests"
            , matchLineSuffix = T.encodeUtf8 " (tests) where"
            , matchOffset     = 304
            }
      xs <- grep' [osp|.|] "^module Data.Filesystem.Grep.Tests" ["*.hs"] False
      checkEqual xs ([expected], SomeFilesMatched)
  , testCase "grep 2" $ do
      let path     = root </> [osp|single-line.txt|]
          expected = MatchEntry
            { matchAbsPath    = AbsFile $ [osp|.|] </> path
            , matchRelPath    = RelFile path
            , matchLineNum    = 1
            , matchColumnNum  = 4
            , matchLinePrefix = T.encodeUtf8 "abc "
            , matchLineStr    = T.encodeUtf8 "foo"
            , matchLineSuffix = T.encodeUtf8 " abc"
            , matchOffset     = 4
            }
      xs <- grep' [osp|.|] "foo" ["single-line.txt"] False
      checkEqual xs ([expected], SomeFilesMatched)
  , testCase "grep unicode 1" $ do
      let path     = root </> [osp|test.txt|]
          expected = MatchEntry
            { matchAbsPath    = AbsFile $ [osp|.|] </> path
            , matchRelPath    = RelFile path
            , matchLineNum    = 2
            , matchColumnNum  = 16
            , matchLinePrefix = T.encodeUtf8 "〚decombobulate"
            , matchLineStr    = T.encodeUtf8 "〛"
            , matchLineSuffix = T.encodeUtf8 ""
            , matchOffset     = 17
            }
      xs <- grep' [osp|.|] "〛" ["*.txt"] False
      checkEqual xs ([expected], SomeFilesMatched)
  , testCase "grep unicode 2" $ do
      let path     = root </> [osp|more-unicode.txt|]
          expected = MatchEntry
            { matchAbsPath    = AbsFile $ [osp|.|] </> path
            , matchRelPath    = RelFile path
            , matchLineNum    = 2
            , matchColumnNum  = 6
            , matchLinePrefix = T.encodeUtf8 "〖〖"
            , matchLineStr    = T.encodeUtf8 "привет мир"
            , matchLineSuffix = T.encodeUtf8 "〗〗"
            , matchOffset     = 8
            }
      xs <- grep' [osp|.|] "привет мир" ["more-unicode.txt"] False
      checkEqual xs ([expected], SomeFilesMatched)
  , testCase "grep unicode 3" $ do
      let path      = root </> [osp|more-unicode.txt|]
          expected1 = MatchEntry
            { matchAbsPath    = AbsFile $ [osp|.|] </> path
            , matchRelPath    = RelFile path
            , matchLineNum    = 2
            , matchColumnNum  = 6
            , matchLinePrefix = T.encodeUtf8 "〖〖"
            , matchLineStr    = T.encodeUtf8 "привет"
            , matchLineSuffix = T.encodeUtf8 " мир〗〗"
            , matchOffset     = 8
            }
          expected2 = MatchEntry
            { matchAbsPath    = AbsFile $ [osp|.|] </> path
            , matchRelPath    = RelFile path
            , matchLineNum    = 2
            , matchColumnNum  = 32
            , matchLinePrefix = T.encodeUtf8 ""
            , matchLineStr    = T.encodeUtf8 "привет"
            , matchLineSuffix = T.encodeUtf8 ", мир"
            , matchOffset     = 34
            }
      xs <- grep' [osp|.|] "привет\\>" ["more-unicode.txt"] False
      checkEqual xs ([expected1, expected2], SomeFilesMatched)
  , testCase "grep multiline" $ do
      let file      = [osp|multiline.txt|]
          expected1 = MatchEntry
            { matchAbsPath    = AbsFile (root </> file)
            , matchRelPath    = RelFile file
            , matchLineNum    = 3
            , matchColumnNum  = 6
            , matchLinePrefix = T.encodeUtf8 "hello "
            , matchLineStr    = T.encodeUtf8 "foo\nbar"
            , matchLineSuffix = T.encodeUtf8 " world"
            , matchOffset     = 11
            }
          expected2 = MatchEntry
            { matchAbsPath    = AbsFile (root </> file)
            , matchRelPath    = RelFile file
            , matchLineNum    = 13
            , matchColumnNum  = 6
            , matchLinePrefix = T.encodeUtf8 "Hello "
            , matchLineStr    = T.encodeUtf8 "Foo\nBar"
            , matchLineSuffix = T.encodeUtf8 " World"
            , matchOffset     = 58
            }
      xs <- grep' root "foo\nbar" ["*.txt"] True
      checkEqual xs ([expected1, expected2], SomeFilesMatched)
  , testCase "grep binary" $ do
      let file      = [osp|binary-data.bin|]
          expected1 = MatchEntry
            { matchAbsPath    = AbsFile $ root </> file
            , matchRelPath    = RelFile file
            , matchLineNum    = 1
            , matchColumnNum  = 999
            , matchLinePrefix = T.encodeUtf8 "./test-data/"
            , matchLineStr    = T.encodeUtf8 "binary"
            , matchLineSuffix = T.encodeUtf8 "-data.bin"
            , matchOffset     = 999
            }
          expected2 = MatchEntry
            { matchAbsPath    = AbsFile $ root </> file
            , matchRelPath    = RelFile file
            , matchLineNum    = 1
            , matchColumnNum  = 1325
            , matchLinePrefix = T.encodeUtf8 "./"
            , matchLineStr    = T.encodeUtf8 "build-both"
            , matchLineSuffix = T.encodeUtf8 ".sh"
            , matchOffset     = 1325
            }
      xs <- grep' root "binary|build-both" ["binary-data.bin"] False
      checkEqual xs ([expected1, expected2], SomeFilesMatched)
  , testCase "grep compressed binary" $ do
      let file      = [osp|binary-data.bin.gz|]
          expected1 = MatchEntry
            { matchAbsPath    = AbsFile $ root </> file
            , matchRelPath    = RelFile file
            , matchLineNum    = 1
            , matchColumnNum  = 999
            , matchLinePrefix = T.encodeUtf8 "./test-data/"
            , matchLineStr    = T.encodeUtf8 "binary"
            , matchLineSuffix = T.encodeUtf8 "-data.bin"
            , matchOffset     = 999
            }
          expected2 = MatchEntry
            { matchAbsPath    = AbsFile $ root </> file
            , matchRelPath    = RelFile file
            , matchLineNum    = 1
            , matchColumnNum  = 1325
            , matchLinePrefix = T.encodeUtf8 "./"
            , matchLineStr    = T.encodeUtf8 "build-both"
            , matchLineSuffix = T.encodeUtf8 ".sh"
            , matchOffset     = 1325
            }
      xs <- grep' root "binary|build-both" ["binary-data.bin.gz"] False
      checkEqual xs ([expected1, expected2], SomeFilesMatched)
  , testCase "grep crlf line endings" $ do
      let file     = [osp|crlf.txt|]
          expected = MatchEntry
            { matchAbsPath    = AbsFile $ root </> file
            , matchRelPath    = RelFile file
            , matchLineNum    = 4
            , matchColumnNum  = 2
            , matchLinePrefix = T.encodeUtf8 "ba"
            , matchLineStr    = T.encodeUtf8 "rcrl"
            , matchLineSuffix = T.encodeUtf8 "f"
            , matchOffset     = 11
            }
      xs <- grep' root "rcrl" ["crlf.txt"] False
      checkEqual xs ([expected], SomeFilesMatched)
  , testCase "grep tabs" $ do
      let file     = [osp|tab.txt|]
          expected = MatchEntry
            { matchAbsPath    = AbsFile $ root </> file
            , matchRelPath    = RelFile file
            , matchLineNum    = 2
            , matchColumnNum  = 5 -- Count tabs as 1 character wide.
            , matchLinePrefix = T.encodeUtf8 "\tfoo\t"
            , matchLineStr    = T.encodeUtf8 "bar\t"
            , matchLineSuffix = T.encodeUtf8 "quux\tdecombobulate\t"
            , matchOffset     = 6
            }
      xs <- grep' root "bar\t" ["tab.txt"] False
      checkEqual xs ([expected], SomeFilesMatched)
  , testGroup "globs match filenames without extensions" $
      let file     = [osp|pippo|]
          expected = MatchEntry
            { matchAbsPath    = AbsFile $ root </> file
            , matchRelPath    = RelFile file
            , matchLineNum    = 3
            , matchColumnNum  = 4 -- Count tabs as 1 character wide.
            , matchLinePrefix = T.encodeUtf8 "123 "
            , matchLineStr    = T.encodeUtf8 "pippo"
            , matchLineSuffix = T.encodeUtf8 " 456"
            , matchOffset     = 6
            }
      in
      [ testCase "glob that matches anything" $ do
          xs <- grep' root "pippo" ["*"] False
          checkEqual xs ([expected], SomeFilesMatched)
      , testCase "glob that matches middle of file name" $ do
          xs <- grep' root "pippo" ["*ipp*"] False
          checkEqual xs ([expected], SomeFilesMatched)
      ]
  , testCase "grep no glob matches" $ do
      xs <- grep' root "bar" ["*.decombobulate"] False
      checkEqual xs ([], NoFilesMatched)
  , testCase "grep no matches but some files matched globs" $ do
      xs <- grep' root "decombobulatedecombobulate" ["*"] False
      checkEqual xs ([], SomeFilesMatched)
  ]
  where
    root = [osp|test-data|]

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

grep' :: OsPath -> Text -> [Text] -> Bool -> IO ([MatchEntry], AnyFilesMatched)
grep' root reToFind globs ignoreCase = runNoEarlyTerminationT $
  first toList <$>
    grep [root] (T.encodeUtf8 reToFind) globs ignoreCase dummyIgnores dummyIgnores (\_ entry -> pure entry)

