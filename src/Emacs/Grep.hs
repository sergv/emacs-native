----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Grep
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Emacs.Grep (initialise) where

import Control.Arrow (first)
import Control.Concurrent.Async.Lifted.Safe
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Control.Exception (finally)
import Control.Monad
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Char8.Ext qualified as C8.Ext
import Data.Foldable
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Ord
import Data.Semigroup as Semi
import Data.Text qualified as T
import Data.Traversable
import Data.Tuple.Homogenous
import Data.Vector (Vector)
import Data.Vector.Unboxed qualified as U
import GHC.Conc (getNumCapabilities)
import Prettyprinter (pretty, (<+>))

import Data.Emacs.Module.Args
import Emacs.Module
import Emacs.Module.Assert
import Emacs.Module.Errors

import Data.Emacs.Module.Doc qualified as Doc
import Data.Emacs.Path
import Data.Filesystem
import Data.Regex
import Path
import Path.IO

initialise
  :: WithCallStack
  => EmacsM s ()
initialise =
  bindFunction "haskell-native-grep-rec" =<<
    makeFunction emacsGrepRec emacsGrepRecDoc

emacsGrepRecDoc :: Doc.Doc
emacsGrepRecDoc =
  "Recursively find files leveraging multiple cores."

emacsGrepRec
  :: forall m s. (WithCallStack, MonadEmacs m, MonadIO (m s), MonadThrow (m s), MonadBaseControl IO (m s), Forall (Pure (m s)), U.Unbox (EmacsRef m s))
  => EmacsFunction ('S ('S ('S ('S ('S ('S 'Z)))))) 'Z 'False s m
emacsGrepRec (R roots (R regexp (R extsGlobs (R ignoredFileGlobs (R ignoredDirGlobs (R ignoreCase Stop)))))) = do
  roots'            <- traverse extractText . U.convert @_ @_ @Vector =<< extractVector roots
  regexp'           <- extractText regexp
  extsGlobs'        <- traverse extractText . U.convert @_ @_ @Vector =<< extractVector extsGlobs
  ignoredFileGlobs' <- traverse extractText . U.convert @_ @_ @Vector =<< extractVector ignoredFileGlobs
  ignoredDirGlobs'  <- traverse extractText . U.convert @_ @_ @Vector =<< extractVector ignoredDirGlobs
  ignoreCase'       <- extractBool ignoreCase

  roots'' <- for roots' $ \root ->
    case parseAbsDir $ T.unpack root of
      Nothing -> throwM $ mkUserError "emacsGrepRec" $
        "One of the search roots is not a valid absolute directory:" <+> pretty root
      Just x  -> do
        exists <- doesDirExist x
        unless exists $
          throwM $ mkUserError "emacsGrepRec" $
            "Search root does not exist:" <+> pretty root
        pure x

  let compOpts =
        defaultCompOpt
          { multiline      = True
          , caseSensitive  = not ignoreCase'
          , lastStarGreedy = True
          }

  regexp'' <- compileReWithOpts compOpts regexp'
  jobs     <- liftBase getNumCapabilities

  extsToFindRE   <- fileGlobsToRegex extsGlobs'
  ignoredFilesRE <- fileGlobsToRegex ignoredFileGlobs'
  ignoredDirsRE  <- fileGlobsToRegex ignoredDirGlobs'

  let shouldVisit :: Path Abs Dir -> Bool
      shouldVisit = not . reMatchesPath ignoredDirsRE
      shouldCollect :: Path Abs Dir -> Path Abs File -> IO [MatchEntry]
      shouldCollect root path
        | reMatchesPath ignoredFilesRE path = pure []
        | Just ext <- fileExtension path
        , reMatchesString extsToFindRE ext = do
            contents <- C8.readFile $ toFilePath path
            case reAllByteStringMatches regexp'' contents of
              AllMatches [] -> pure []
              AllMatches ms -> makeMatches root path ms contents
        | otherwise = pure []

  let collectEntries :: TMQueue MatchEntry -> m s (EmacsRef m s)
      collectEntries results = go mempty
        where
          -- Accumulator is a map from file names and positions within file
          -- to Emacs strings that could be presented to the user.
          go :: Map (C8.ByteString, Word) (EmacsRef m s) -> m s (EmacsRef m s)
          go !acc = do
            res <- liftBase $ atomically $ readTMQueue results
            case res of
              Nothing ->
                makeVector $ toList acc
              Just MatchEntry{matchAbsPath, matchRelPath, matchPos, matchLineNum, matchLinePrefix, matchLineStr, matchLineSuffix} -> do
                let relPathBS = pathForEmacs matchRelPath
                pathEmacs        <- makeString $ pathForEmacs matchAbsPath
                shortPathEmacs   <- makeString relPathBS
                matchLineNum'    <- makeInt (fromIntegral matchLineNum)
                matchPos'        <- makeInt (fromIntegral matchPos)
                matchLinePrefix' <- makeString matchLinePrefix
                matchLineStr'    <- makeString matchLineStr
                matchLineSuffix' <- makeString matchLineSuffix
                emacsMatchStruct <-
                  funcallPrimitiveSym
                    "make-egrep-match"
                    (Tuple7 (pathEmacs, shortPathEmacs, matchLineNum', matchPos', matchLinePrefix', matchLineStr', matchLineSuffix'))
                go $ M.insert (relPathBS, matchPos) emacsMatchStruct acc

  results <- liftBase newTMQueueIO
  let collect :: MatchEntry -> IO ()
      collect = atomically . writeTMQueue results

      doFind =
        findRec FollowSymlinks jobs
          shouldVisit
          shouldCollect
          collect
          roots''

  withAsync (liftBase (doFind `finally` atomically (closeTMQueue results))) $ \searchAsync ->
    collectEntries results <* wait searchAsync

data MatchEntry = MatchEntry
  { matchAbsPath    :: !(Path Abs File)
  , matchRelPath    :: !(Path Rel File)
  , matchPos        :: !Word
  , matchLineNum    :: !Word
  , -- | What comes before the matched text on the relevant line.
    -- Contains no newlines.
    matchLinePrefix :: !C8.ByteString
  , -- | The text that was matched. May contain newlines since we
    -- support multiline matches.
    matchLineStr    :: !C8.ByteString
  , -- | What comes after the matched text on the relevant line.
    -- Contains no newlines.
    matchLineSuffix :: !C8.ByteString
  } deriving (Show)

data MatchState = MatchState
  { msPos     :: !Word
  , msLine    :: !Word
  , msCol     :: !Word
  , msMatches :: [(MatchOffset, MatchLength)]
  , msResult  :: [MatchEntry]
  }

isNewline :: Char -> Bool
isNewline = \case
  '\n' -> True
  '\r' -> True
  _    -> False

makeMatches
  :: MonadThrow m
  => Path Abs Dir  -- ^ Directory where recursive search was initiated
  -> Path Abs File -- ^ Matched file under the directory
  -> [(MatchOffset, MatchLength)]
  -> C8.ByteString
  -> m [MatchEntry]
makeMatches searchRoot fileAbsPath ms str =
  case stripProperPrefix searchRoot fileAbsPath of
    Nothing -> throwM $ mkUserError "emacsGrepRec" $
      "Internal error: findRec produced wrong root for path" <+> pretty (toFilePath fileAbsPath) Semi.<>
      ". The root is" <+> pretty (toFilePath searchRoot)
    Just relPath ->
      pure $ msResult $ C8.foldl' (\acc c -> accumulateMatch $ bumpPos acc c) initState str
      where
        initState = MatchState
          { msPos     = 0
          , msLine    = 1 -- Emacs starts to count lines from 1.
          , msCol     = 0
          , msMatches = L.sortBy (comparing fst) ms
          , msResult  = []
          }
        bumpPos :: MatchState -> Char -> MatchState
        bumpPos s@MatchState{msMatches = []} _    = s
        bumpPos s@MatchState{msPos}          '\r' = s { msPos = msPos + 1 }
        bumpPos s@MatchState{msPos, msLine}  '\n' = s { msPos = msPos + 1, msLine = msLine + 1, msCol = 0 }
        bumpPos s@MatchState{msPos, msCol}   _    = s { msPos = msPos + 1, msCol  = msCol + 1 }

        accumulateMatch :: MatchState -> MatchState
        accumulateMatch s@MatchState{msMatches = []} = s
        accumulateMatch s@MatchState{msPos, msLine, msCol, msMatches = remainingMatches@((offset, _) : _), msResult}
          | msPos' == offset
          = s { msMatches = remainingMatches', msResult = newEntries ++ msResult }
          | otherwise
          = s
          where
            msPos' = fi msPos
            (currentMatches, remainingMatches') =
              first (map snd) $ span ((== msPos') . fst) remainingMatches
            newEntries =
              [ MatchEntry
                 { matchAbsPath    = fileAbsPath
                 , matchRelPath    = relPath
                 , matchPos        = msPos
                 , matchLineNum    = msLine
                 , matchLinePrefix = C8.copy prefix
                 , matchLineStr    = C8.copy matched
                 , matchLineSuffix = C8.copy suffix
                 }
              | len <- currentMatches
              , let (prefix,  rest)  = C8.Ext.splitAt (fi msCol) $ C8.Ext.drop (fi msPos - fi msCol) str
              , let (matched, rest') = C8.Ext.splitAt len rest
              , let suffix           = C8.takeWhile (not . isNewline) rest'
              ]
            fi :: Word -> Int
            fi = fromIntegral
