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

module EmacsNativeHelper (main) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.NoEarlyTermination
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Foldable (traverse_)
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

import Data.Filesystem.Find
import Data.Filesystem.Find.Types
import Data.Filesystem.Grep qualified as Grep
import Data.Ignores

data Command
  = Grep GrepConfig
  | Find FindConfig

readOsPath :: ReadM OsPath
readOsPath = eitherReader (bimap show id . encodeUtf)

data IgnoresConfig = IgnoresConfig
  { cfgIgnoredFileGlobs   :: ![Text]
  , cfgIgnoredDirGlobs    :: ![Text]
  , cfgIgnoredDirPrefixes :: ![Text]
  , cfgIgnoredAbsDirs     :: ![Text]
  }
  deriving (Generic)
  deriving Pretty via PPGeneric IgnoresConfig

ignoresParser :: Parser IgnoresConfig
ignoresParser = do
  cfgIgnoredFileGlobs   <- many $ strOption $
    long "ignore-file-glob" <>
      metavar "GLOB" <>
        help "Filename globs to ignore, e.g. *.txt"

  cfgIgnoredDirGlobs    <- many $ strOption $
    long "ignore-dir-glob" <>
      metavar "GLOB" <>
        help "Directory globs to not descend into during recursive search, e.g. *.txt"

  cfgIgnoredDirPrefixes <- many $ strOption $
    long "ignore-dir-prefix" <>
      metavar "STR" <>
        help "Ignore directories starting with these strings during recusive search"

  cfgIgnoredAbsDirs     <- many $ strOption $
    long "ignore-dir-abs" <>
      metavar "ABS-PATH" <>
        help "Ignore these directories during recusive search"

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
        ( command "grep" (Grep <$> grepProgInfo) <>
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
  (fileIgnores, dirIgnores) <- mkFullIgnores fcfgIgnores
  absRoots <- traverse makeAbsolute fcfgRoots
  traverse_ (T.putStrLn . pathToText) =<<
    fastFileSearch
      (if fcfgIsRelativePaths then ProduceRelativePaths else ProduceAbsolutePaths)
      fileIgnores
      dirIgnores
      fcfgGlobsToFind
      (coerce absRoots :: [AbsDir])

mkFullIgnores :: MonadThrow m => IgnoresConfig -> m (Ignores, Ignores)
mkFullIgnores IgnoresConfig{cfgIgnoredFileGlobs, cfgIgnoredDirGlobs, cfgIgnoredDirPrefixes, cfgIgnoredAbsDirs} = do
  fileIgnores <- mkIgnores cfgIgnoredFileGlobs []
  dirIgnores  <- mkIgnores cfgIgnoredAbsDirs (coerce (cfgIgnoredDirGlobs ++ map (<> "*") cfgIgnoredDirPrefixes))
  pure (fileIgnores, dirIgnores)

grep :: GrepConfig -> IO ()
grep GrepConfig{gcfgRoots, gcfgRegexp, gcfgGlobsToFind, gcfgIgnoreCase, gcfgIgnores} = do
  (fileIgnores, dirIgnores) <- mkFullIgnores gcfgIgnores

  (res, anyMatched) <- runNoEarlyTerminationT $
    Grep.grep gcfgRoots gcfgRegexp gcfgGlobsToFind gcfgIgnoreCase fileIgnores dirIgnores $
      \relPath match -> pure (relPath, match)

  case anyMatched of
    Grep.NoFilesMatched   -> putStrLn "No matches"
    Grep.SomeFilesMatched -> PP.putDocLn $ PP.pretty res

  pure ()
