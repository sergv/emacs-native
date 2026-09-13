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

module EmacsNativeGrepMain (main) where

import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Text (Text)
import Options.Applicative
import Prettyprinter.Combinators qualified as PP
import Prettyprinter.Generics
import Prettyprinter.Instances ()
import System.Directory.OsPath.Types (Basename(..))
import System.OsPath

import Control.Monad.NoEarlyTermination
import Data.Filesystem.Grep
import Data.Ignores

data Config = Config
  { cfgRoots              :: ![OsPath]
  , cfgRegexp             :: !ByteString
  , cfgGlobsToFind        :: ![Text]
  , cfgIgnoredFileGlobs   :: ![Text]
  , cfgIgnoredDirGlobs    :: ![Text]
  , cfgIgnoredDirPrefixes :: ![Text]
  , cfgIgnoredAbsDirs     :: ![Text]
  , cfgIgnoreCase         :: !Bool
  }
  deriving Generic
  deriving Pretty via PPGeneric Config

optsParser :: Parser Config
optsParser = do
  cfgGlobsToFind        <- many $ strOption $
    short 'g' <>
      long "glob" <>
        metavar "GLOB" <>
          help "Filename globs to search for, e.g. *.txt"

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

  cfgIgnoreCase         <- switch $
    long "ignore-case" <>
      help "Ignore character case during matching, i.e. match specified regexp case-insensitively"

  cfgRegexp             <- strArgument $
    metavar "REGEXP" <>
    help "Regexp to search for"

  cfgRoots              <- many $ argument readOsPath $
    metavar "DIRECTORY" <>
    help "Directories to recursively search in"

  pure Config{..}
  where
    readOsPath = eitherReader (bimap show id . encodeUtf)

progInfo :: ParserInfo Config
progInfo = info
  (helper <*> optsParser)
  (fullDesc <> header "Grep just like in libemacs-native.so but available from command line for asynchronous execution, profiling, or debugging")

main :: IO ()
main = do
  Config{cfgRoots, cfgRegexp, cfgGlobsToFind, cfgIgnoredFileGlobs, cfgIgnoredDirGlobs, cfgIgnoredDirPrefixes, cfgIgnoredAbsDirs, cfgIgnoreCase} <-
    customExecParser (prefs (showHelpOnEmpty <> noBacktrack <> multiSuffix "*")) progInfo

  fileIgnores <- mkIgnores cfgIgnoredFileGlobs []
  dirIgnores  <- mkIgnores cfgIgnoredAbsDirs (coerce (cfgIgnoredDirGlobs ++ map (<> "*") cfgIgnoredDirPrefixes))

  (res, anyMatched) <- runNoEarlyTerminationT $
    grep cfgRoots cfgRegexp cfgGlobsToFind cfgIgnoreCase fileIgnores dirIgnores $
      \relPath match -> pure (relPath, match)

  case anyMatched of
    NoFilesMatched   -> putStrLn "No matches"
    SomeFilesMatched -> PP.putDocLn $ PP.pretty res

  pure ()
