----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Grep
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Emacs.Grep (initialise) where

import Control.Arrow (first)
import Control.Concurrent
import Control.Concurrent.Async.Lifted.Safe
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Char8.Ext qualified as C8.Ext
import Data.Coerce
import Data.Foldable
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Ord
import Data.Semigroup as Semi
import Data.Tuple.Homogenous
import Prettyprinter (pretty, (<+>))
import System.Directory.OsPath
import System.File.OsPath as OsPath
import System.OsPath

import Data.Emacs.Module.Args
import Emacs.Module
import Emacs.Module.Assert
import Emacs.Module.Errors

import Data.Emacs.Module.Doc qualified as Doc
import Data.Emacs.Path
import Data.Filesystem
import Data.Ignores
import Data.Regex
import Emacs.EarlyTermination
import Emacs.Module.Monad qualified as Emacs

initialise
  :: WithCallStack
  => Emacs.EmacsM s ()
initialise =
  bindFunction "haskell-native-grep" =<<
    makeFunction emacsGrepRec emacsGrepRecDoc

emacsGrepRecDoc :: Doc.Doc
emacsGrepRecDoc =
  "Recursively find files leveraging multiple cores."

emacsGrepRec
  :: forall m v s.
     ( WithCallStack
     , MonadEmacs m v
     , MonadIO (m s)
     , MonadBaseControl IO (m s)
     , Forall (Pure (m s))
     , forall ss. MonadThrow (m ss)
     )
  => EmacsFunction ('S ('S ('S ('S ('S ('S ('S ('S 'Z)))))))) 'Z 'False m v s
emacsGrepRec (R roots (R regexp (R globsToFind (R ignoredFileGlobs (R ignoredDirGlobs (R ignoredDirPrefixes (R ignoredAbsDirs (R ignoreCase Stop)))))))) = do
  roots'       <- extractListWith extractOsPath roots
  regexp'      <- extractText regexp
  globsToFind' <- extractListWith extractText globsToFind
  ignoreCase'  <- extractBool ignoreCase
  ignores      <- mkIgnores ignoredFileGlobs ignoredDirGlobs ignoredDirPrefixes ignoredAbsDirs

  for_ roots' $ \root -> do
    exists <- liftBase $ doesDirectoryExist root
    unless exists $
      throwM $ mkUserError "emacsGrepRec" $
        "Search root does not exist:" <+> pretty (pathToText root)

  let compOpts =
        defaultCompOpt
          { multiline      = True
          , caseSensitive  = not ignoreCase'
          , lastStarGreedy = True
          }

  regexp'' <- compileReWithOpts compOpts regexp'
  jobs     <- liftBase getNumCapabilities

  extsToFindRE <- fileGlobsToRegex globsToFind'

  let shouldCollect :: AbsDir -> AbsFile -> RelFile -> IO [MatchEntry]
      shouldCollect root absPath'@(AbsFile absPath) (RelFile relPath)
        | isIgnoredFile ignores absPath' = pure []
        | hasExtension absPath
        , reMatches extsToFindRE $ pathToText $ takeExtension relPath = do
            contents <- OsPath.readFile' absPath
            case reAllByteStringMatches regexp'' contents of
              AllMatches [] -> pure []
              AllMatches ms -> makeMatches root absPath' ms contents
        | otherwise = pure []

  results <- liftBase newTMQueueIO
  let collect :: MatchEntry -> IO ()
      collect = atomically . writeTMQueue results

      doFind :: IO ()
      doFind =
        findRec FollowSymlinks jobs
          (shouldVisit ignores)
          shouldCollect
          collect
          (coerce roots' :: [AbsDir])

  withAsync (liftBase (doFind `finally` atomically (closeTMQueue results))) $ \searchAsync -> do
    matches <- consumeTMQueueWithEarlyTermination results mempty $
      \ !acc MatchEntry{matchAbsPath, matchRelPath, matchLineNum, matchColumnNum, matchLinePrefix, matchLineStr, matchLineSuffix} -> do
        let !relPathBS = pathForEmacs $ unRelFile matchRelPath
            !key       = (relPathBS, matchLineNum)
            f :: Maybe (v s) -> m s (Maybe (v s))
            f = \case
              x@Just{} -> pure x
              Nothing  -> do
                !pathEmacs        <- makeShortByteString $ pathForEmacs $ unAbsFile matchAbsPath
                !shortPathEmacs   <- makeShortByteString relPathBS
                !matchLineNum'    <- makeInt (fromIntegral matchLineNum)
                !matchColumnNum'  <- makeInt (fromIntegral matchColumnNum)
                !matchLinePrefix' <- makeString' matchLinePrefix
                !matchLineStr'    <- makeString' matchLineStr
                !matchLineSuffix' <- makeString' matchLineSuffix
                !emacsMatchStruct <-
                  funcallPrimitiveSym
                    "make-egrep-match"
                    (Tuple7 (pathEmacs, shortPathEmacs, matchLineNum', matchColumnNum', matchLinePrefix', matchLineStr', matchLineSuffix'))
                pure $ Just emacsMatchStruct
        M.alterF f key acc
    wait searchAsync
    makeList matches

makeString'
  :: MonadEmacs m v
  => C8.ByteString
  -> m s (v s)
makeString' x
  | BS.isValidUtf8 x = makeString x
  | otherwise        = makeBinaryString x

data MatchEntry = MatchEntry
  { matchAbsPath    :: !AbsFile
  , matchRelPath    :: !RelFile
  , matchLineNum    :: !Word
  , matchColumnNum  :: !Word
  , -- | What comes before the matched text on the relevant line.
    -- Contains no newlines.
    matchLinePrefix :: !C8.ByteString
  , -- | The text that was matched. May contain newlines since we
    -- support multiline matches.
    matchLineStr    :: !C8.ByteString
  , -- | What comes after the matched text on the relevant line.
    -- Contains no newlines.
    matchLineSuffix :: !C8.ByteString
  }

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
  => AbsDir  -- ^ Directory where recursive search was initiated
  -> AbsFile -- ^ Matched file under the directory
  -> [(MatchOffset, MatchLength)]
  -> C8.ByteString
  -> m [MatchEntry]
makeMatches (AbsDir searchRoot) fileAbsPath'@(AbsFile fileAbsPath) ms str =
  case stripProperPrefix searchRoot fileAbsPath of
    Nothing -> throwM $ mkUserError "emacsGrepRec" $
      "Internal error: findRec produced wrong root for path" <+> pretty (pathToText fileAbsPath) Semi.<>
      ". The root is" <+> pretty (pathToText searchRoot)
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
                 { matchAbsPath    = fileAbsPath'
                 , matchRelPath    = RelFile relPath
                 , matchLineNum    = msLine
                 , matchColumnNum  = msCol
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
