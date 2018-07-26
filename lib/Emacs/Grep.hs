----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.Grep
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Emacs.Grep (initialise) where

import Control.Arrow (first)
import Control.Concurrent.Async.Lifted.Safe
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Control.Exception (finally)
import qualified Control.Exception.Safe.Checked as Checked
import Control.Monad
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.Control

import Data.ByteString.Lex.Integral (packDecimal)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Char8.Ext as C8.Ext
import Data.Foldable
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord
import Data.Semigroup as Semi
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc (pretty, (<+>))
import Data.Traversable
import GHC.Conc (getNumCapabilities)

import Data.Emacs.Module.Args
import Data.Emacs.Module.SymbolName.TH
import Emacs.Module
import Emacs.Module.Assert
import Emacs.Module.Errors

import Data.Filesystem
import Data.Regex
import Path
import Path.IO

initialise
  :: (WithCallStack, Throws EmacsThrow, Throws EmacsError, Throws EmacsInternalError)
  => EmacsM s ()
initialise =
  bindFunction [esym|haskell-native-grep-rec|] =<<
    makeFunction emacsGrepRec emacsGrepRecDoc

emacsGrepRecDoc :: C8.ByteString
emacsGrepRecDoc =
  "Recursively find files leveraging multiple cores."

emacsGrepRec
  :: forall m s. (WithCallStack, MonadEmacs m, Monad (m s), MonadIO (m s), MonadThrow (m s), MonadBaseControl IO (m s), Forall (Pure (m s)))
  => EmacsFunction ('S ('S ('S ('S ('S ('S 'Z)))))) 'Z 'False s m
emacsGrepRec (R roots (R regexp (R extsGlobs (R ignoredFileGlobs (R ignoredDirGlobs (R ignoreCase Stop)))))) = do
  roots'            <- extractVectorWith extractText roots
  regexp'           <- extractText regexp
  extsGlobs'        <- extractVectorWith extractText extsGlobs
  ignoredFileGlobs' <- extractVectorWith extractText ignoredFileGlobs
  ignoredDirGlobs'  <- extractVectorWith extractText ignoredDirGlobs
  ignoreCase'       <- extractBool ignoreCase

  roots'' <- for roots' $ \root ->
    case parseAbsDir $ T.unpack root of
      Nothing -> Checked.throw $ mkUserError "emacsGrepRec" $
        "One of the search roots is not a valid absolute directory:" <+> pretty root
      Just x  -> do
        exists <- doesDirExist x
        unless exists $
          Checked.throw $ mkUserError "emacsGrepRec" $
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

  extsToFindRE   <- globsToRegex extsGlobs'
  ignoredFilesRE <- globsToRegex ignoredFileGlobs'
  ignoredDirsRE  <- globsToRegex ignoredDirGlobs'

  let shouldVisit :: Path Abs Dir -> Bool
      shouldVisit = not . reMatchesPath ignoredDirsRE
      shouldCollect :: Path Abs Dir -> Path Abs File -> IO [MatchEntry]
      shouldCollect root path
        | reMatchesPath ignoredFilesRE path = pure []
        | reMatchesString extsToFindRE (fileExtension path) = do
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
          go acc = do
            res <- liftBase $ atomically $ readTMQueue results
            case res of
              Nothing ->
                makeVector $ toList acc
              Just x@MatchEntry{matchPos} -> do
                (pathBS, pathEmacs, formatted) <- formatMatchEntry x
                matchPos'        <- makeInt (fromIntegral matchPos)
                emacsMatchStruct <- funcallPrimitive [esym|make-egrep-match|] [pathEmacs, matchPos', formatted]
                go $! M.insert (pathBS, matchPos) emacsMatchStruct acc

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
    (produceRef =<< collectEntries results) <* wait searchAsync

data MatchEntry = MatchEntry
  { matchAbsPath    :: !(Path Abs File)
  , matchRelPath    :: !(Path Rel File)
  , matchPos        :: !Word
  , matchLineNum    :: !Word
  , matchColumn     :: !Word
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

-- Connect last element of the first list and head of the second list.
connectTailHeadWith :: (a -> a -> a) -> [a] -> [a] -> [a]
connectTailHeadWith f xs ys = go xs
  where
    go = \case
      []      -> ys
      xs'@[x] ->
        case ys of
          []      -> xs'
          y : ys' -> f x y : ys'
      (x:xs') -> x : go xs'

-- | Create a string from a match entry that will present a match to
-- the user.
formatMatchEntry
  :: forall m s. (Monad (m s), MonadThrow (m s), MonadEmacs m, Throws UserError)
  => MatchEntry -> m s (C8.ByteString, EmacsRef m s, EmacsRef m s)
formatMatchEntry MatchEntry{matchAbsPath, matchRelPath, matchLineNum, matchLinePrefix, matchLineStr, matchLineSuffix} = do
  let matchPath'    = C8.pack $ toFilePath matchRelPath
      matchLineNum' = packWord matchLineNum
  emacsPath <- makeString $ C8.pack $ toFilePath matchAbsPath
  fileName  <- (`addFaceProp` [esym|compilation-info|])        =<< makeString matchPath'
  lineNum   <- (`addFaceProp` [esym|compilation-line-number|]) =<< makeString matchLineNum'
  colon     <- makeString ":"

  lineNum' <- addFaceProp lineNum [esym|compilation-line-number|]

  let prefixLines, matchedLines, suffixLines :: [m s (EmacsRef m s)]
      prefixLines  = [makeString matchLinePrefix | not $ C8.null matchLinePrefix ]
      matchedLines = [ (`addFaceProp` [esym|lazy-highlight|]) =<< makeString line
                     | line <- C8.lines matchLineStr
                     ]
      suffixLines  = [makeString (C8.snoc matchLineSuffix '\n')]

      connect
        :: [m s (EmacsRef m s)]
        -> [m s (EmacsRef m s)]
        -> [m s (EmacsRef m s)]
      connect = connectTailHeadWith $ \x y -> do
        x' <- x
        y' <- y
        concat2 x' y'

  matchedTextLines <- sequence $ connect prefixLines $ connect matchedLines suffixLines

  let paddingSize = C8.length matchPath' + 1 + C8.length matchLineNum'
  headerPadding <- makeString $ C8.replicate paddingSize ' '

  let body = L.intersperse headerPadding matchedTextLines
  formatted <- funcallPrimitive [esym|concat|] $ [fileName, colon, lineNum', colon] ++ body
  pure (matchPath', emacsPath, formatted)


{-# INLINE packWord #-}
packWord :: Word -> C8.ByteString
packWord = fromMaybe err . packDecimal
  where
    err = error "Impossible: a Word value cannot be negative"

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
  :: (Throws UserError, MonadThrow m)
  => Path Abs Dir
  -> Path Abs File
  -> [(MatchOffset, MatchLength)]
  -> C8.ByteString
  -> m [MatchEntry]
makeMatches root absPath ms str =
  case stripProperPrefix root absPath of
    Nothing -> Checked.throw $ mkUserError "emacsGrepRec" $
      "Internal error: findRec produced wrong root for path" <+> pretty (toFilePath absPath) Semi.<>
      ". The root is" <+> pretty (toFilePath root)
    Just relPath ->
      pure $ reverse $ msResult $ C8.foldl' (\acc c -> accumulateMatch $ bumpPos acc c) initState str
      where
        initState = MatchState
          { msPos     = 0
          , msLine    = 0
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
                 { matchAbsPath    = absPath
                 , matchRelPath    = relPath
                 , matchPos        = msPos + 1 -- Emacs starts to count positions from 1.
                 , matchLineNum    = msLine + 1 -- -- Emacs starts to count lines from 1.
                 , matchColumn     = msCol
                 , matchLinePrefix = C8.copy prefix
                 , matchLineStr    = C8.copy matched
                 , matchLineSuffix = C8.copy suffix
                 }
              | len <- currentMatches
              , let (prefix,  rest)  = C8.Ext.splitAt (fi msCol) $ C8.Ext.drop (fi msPos - fi msCol) str
              , let (matched, rest') = C8.Ext.splitAt len rest
              , let suffix           = C8.takeWhile (not . isNewline) rest'
              ]
            fi = fromIntegral
