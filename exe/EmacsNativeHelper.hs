-- |
-- Module:     EmacsNativeGrepMain
-- Copyright:  (c) Sergey Vinokurov 2026
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module EmacsNativeHelper (main) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.NoEarlyTermination
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Foldable (traverse_, toList)
import Data.Text (Text)
import Data.Text.IO qualified as T
import Options.Applicative
import Prettyprinter.Combinators qualified as PP
import Prettyprinter.Generics
import Prettyprinter.Instances ()
import System.Directory.OsPath (makeAbsolute)
import System.Directory.OsPath.Types (Basename(..))
import System.OsPath
import System.OsPath.Ext

import Data.ElispSexp qualified as Sexp
import Data.Filesystem.Find
import Data.Filesystem.Find.Types
import Data.Filesystem.Grep qualified as Grep
import Data.Ignores

data Command
  = Grep GrepConfig
  | Find FindConfig

readOsPath :: ReadM OsPath
readOsPath = eitherReader (bimap show id . encodeUtf)

instance Pretty a => Pretty (Basename a) where
  pretty = pretty . unBasename

data IgnoresConfig = IgnoresConfig
  { cfgIgnoredFileBasenameGlobs :: ![Basename Text]
  , cfgIgnoredFileAbsGlobs      :: ![Text]
  , cfgIgnoredDirBasenameGlobs  :: ![Basename Text]
  , cfgIgnoredDirAbsGlobs       :: ![Text]
  }
  deriving (Generic)
  deriving Pretty via PPGeneric IgnoresConfig

ignoresParser :: Parser IgnoresConfig
ignoresParser = do
  cfgIgnoredFileBasenameGlobs <- many $ fmap Basename $ strOption $
    long "ifbg" <>
      long "ignore-file-basename-glob" <>
        metavar "GLOB" <>
          help "File globs that match basename paths to ignore, e.g. *.txt"

  cfgIgnoredFileAbsGlobs      <- many $ strOption $
    long "ifag" <>
      long "ignore-file-abs-glob" <>
        metavar "GLOB" <>
          help "File globs that match absolute paths to ignore, e.g. ~/f*o/*.txt"

  cfgIgnoredDirBasenameGlobs <- many $ fmap Basename $ strOption $
    long "idbg" <>
      long "ignore-dir-basename-glob" <>
        metavar "GLOB" <>
          help "Directory globs that match basename paths to ignore, e.g. bar*, ba*z/"

  cfgIgnoredDirAbsGlobs      <- many $ strOption $
    long "idag" <>
      long "ignore-dir-abs-glob" <>
        metavar "GLOB" <>
          help "Directory globs that match absolute paths to ignore, e.g. ~/foo/bar*, ~/foo/ba*z/"

  pure IgnoresConfig{..}

data FindConfig = FindConfig
  { fcfgRoots           :: ![OsPath]
  , fcfgGlobsToFind     :: ![Text]
  , fcfgIgnores         :: !IgnoresConfig
  , fcfgIsRelativePaths :: !Bool
  }
  deriving Generic
  deriving Pretty via PPGeneric FindConfig

findParser :: Parser FindConfig
findParser = do
  fcfgGlobsToFind     <- many $ strOption $
    short 'g' <>
      long "glob" <>
        metavar "GLOB" <>
          help "Filename globs to search for, e.g. *.txt"

  fcfgIgnores         <- ignoresParser

  fcfgIsRelativePaths <- switch $
    long "relative-paths" <>
      help "Print result names relative to ROOT"

  fcfgRoots           <- many $ argument readOsPath $
    metavar "ROOT" <>
    help "Directory to recursively search in"

  pure FindConfig{..}

findProgInfo :: ParserInfo FindConfig
findProgInfo = info
  (helper <*> findParser)
  (fullDesc <> header "Find just like in libemacs-native.so but available from command line for asynchronous execution, profiling, or debugging")

data GrepConfig = GrepConfig
  { gcfgRoots       :: ![OsPath]
  , gcfgRegexp      :: !ByteString
  , gcfgGlobsToFind :: ![Text]
  , gcfgIgnores     :: !IgnoresConfig
  , gcfgIgnoreCase  :: !Bool
  , gcfgSexpOutput  :: !Bool
  }
  deriving Generic
  deriving Pretty via PPGeneric GrepConfig

grepParser :: Parser GrepConfig
grepParser = do
  gcfgGlobsToFind       <- many $ strOption $
    short 'g' <>
      long "glob" <>
        metavar "GLOB" <>
          help "Filename globs to search for, e.g. *.txt"

  gcfgIgnoreCase        <- switch $
    long "ignore-case" <>
      help "Ignore character case during matching, i.e. match specified regexp case-insensitively"

  gcfgSexpOutput        <- switch $
    long "sexp-output" <>
      help "Format results as S-expression for easier reading by emacs"

  gcfgIgnores           <- ignoresParser

  gcfgRegexp            <- strArgument $
    metavar "REGEXP" <>
    help "Regexp to search for"

  gcfgRoots             <- many $ argument readOsPath $
    metavar "DIRECTORY" <>
    help "Directories to recursively search in"

  pure GrepConfig{..}

grepProgInfo :: ParserInfo GrepConfig
grepProgInfo = info
  (helper <*> grepParser)
  (fullDesc <> header "Grep just like in libemacs-native.so but available from command line for asynchronous execution, profiling, or debugging")

progInfo :: ParserInfo Command
progInfo =
  info
    (helper <*>
      subparser
        (command "grep" (Grep <$> grepProgInfo) <>
          command "find" (Find <$> findProgInfo)))
    -- todo: C-' in Emacs doesn’t insert "’" (the typographic single quote) in strings.
    -- In comments regular "'" inserts typographic quotes,  C-' inserts regular quotes
    -- In strings it should be the other way around.
    -- In regular Haskell code only ' should be inserted.
    (fullDesc <> header "Executable that implements backend for native-accelerated emacs functions like grep and find that can be started as a subprocess instead of an .so module for e.g. asynchronous non-blocking invocation or for executing on a remote host over ssh through TRAMP")

main :: IO ()
main = do
  cfg <-
    customExecParser (prefs (showHelpOnEmpty <> noBacktrack <> multiSuffix "*")) progInfo

  case cfg of
    Grep cfg' -> grep cfg'
    Find cfg' -> find cfg'

find :: FindConfig -> IO ()
find FindConfig{fcfgRoots, fcfgGlobsToFind, fcfgIgnores, fcfgIsRelativePaths} = do
  ignores  <- mkFullIgnores' fcfgIgnores
  absRoots <- traverse makeAbsolute fcfgRoots
  traverse_ (T.putStrLn . pathToText) =<<
    fastFileSearch
      (if fcfgIsRelativePaths then ProduceRelativePaths else ProduceAbsolutePaths)
      ignores
      fcfgGlobsToFind
      (coerce absRoots :: [AbsDir])

mkFullIgnores' :: MonadThrow m => IgnoresConfig -> m FullIgnores
mkFullIgnores' IgnoresConfig{cfgIgnoredFileBasenameGlobs, cfgIgnoredFileAbsGlobs, cfgIgnoredDirBasenameGlobs, cfgIgnoredDirAbsGlobs} = do
  mkFullIgnores cfgIgnoredFileBasenameGlobs cfgIgnoredFileAbsGlobs cfgIgnoredDirBasenameGlobs cfgIgnoredDirAbsGlobs

grep :: GrepConfig -> IO ()
grep GrepConfig{gcfgRoots, gcfgRegexp, gcfgGlobsToFind, gcfgIgnoreCase, gcfgSexpOutput, gcfgIgnores} = do
  ignores <- mkFullIgnores' gcfgIgnores

  gcfgRoots' <- traverse makeAbsolute gcfgRoots

  (res, anyMatched) <- runNoEarlyTerminationT $
    Grep.grep gcfgRoots' gcfgRegexp gcfgGlobsToFind gcfgIgnoreCase ignores $
      \relPath match -> pure (relPath, match)

  if gcfgSexpOutput
  then
    T.putStrLn $ Sexp.renderSexp $
      case anyMatched of
        Grep.NoFilesMatched   ->
          Sexp.Symbol "no-files-matched"
        Grep.SomeFilesMatched ->
          Sexp.toSexp $ map snd $ toList res
  else
    case anyMatched of
      Grep.NoFilesMatched   -> putStrLn "No matches"
      Grep.SomeFilesMatched ->
        PP.putDocLn $ PP.pretty res

  pure ()

