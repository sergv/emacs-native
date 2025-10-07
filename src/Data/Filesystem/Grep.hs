-- |
-- Module:     Data.Filesystem.Grep
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Data.Filesystem.Grep
  ( grep
  , MatchEntry(..)
  ) where

import Control.Arrow (first)
import Control.Concurrent
import Control.Concurrent.Async.Lifted.Safe
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short (ShortByteString)
import Data.Coerce
import Data.Foldable
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Ord
import Data.Semigroup as Semi
import Data.Text (Text)
import Data.Word (Word8)
import GHC.Base (unsafeChr)
import Prettyprinter
import Prettyprinter.Generics
import System.Directory.OsPath.Types
import System.File.OsPath as OsPath
import System.OsPath
import System.OsPath.Ext
import Data.Int (Int64)

import Emacs.Module
import Emacs.Module.Errors

import Data.Emacs.Path
import Data.Filesystem.Find
import Data.Ignores
import Data.Regex
import Data.UnicodeUtils
import Emacs.EarlyTermination

grep
  :: forall m s v a. (MonadEmacs m v, forall ss. MonadThrow (m ss), MonadBaseControl IO (m s), Forall (Pure (m s)))
  => [OsPath]
  -> Text
  -> [Text]
  -> Bool
  -> Ignores
  -> Ignores
  -> (ShortByteString -> MatchEntry -> m s a)
  -> m s (Map (ShortByteString, Word) a)
grep roots regexp globsToFind ignoreCase fileIgnores dirIgnores f = do
  let compOpts =
        defaultCompOpt
          { multiline      = True
          , caseSensitive  = not ignoreCase
          , lastStarGreedy = True
          }

  regexp' <- compileReWithOptsUnicodeAsBytes compOpts regexp
  jobs    <- liftBase getNumCapabilities

  extsToFindRE <- fileGlobsToRegex globsToFind

  let searchFile :: AbsDir -> AbsFile -> Relative OsPath -> Basename OsPath -> IO (Maybe [MatchEntry])
      searchFile root absPath _ (Basename basePath)
        | isIgnoredFile fileIgnores absPath = pure Nothing
        | hasExtension (unAbsFile absPath)
        , reMatches extsToFindRE $ pathToText basePath = do
            contents <- OsPath.readFile (unAbsFile absPath)
            case reAllByteStringMatches regexp' contents of
              AllMatches [] -> pure Nothing
              AllMatches ms -> Just <$> makeMatches root absPath ms contents
        | otherwise = pure Nothing

  results <- liftBase newTMQueueIO
  let collect :: [MatchEntry] -> IO ()
      collect = traverse_ (atomically . writeTMQueue results)

      doFind :: IO ()
      doFind =
        traverse_ collect =<< findRec FollowSymlinks jobs
          (\x y -> not $ isIgnored dirIgnores x y)
          searchFile
          (coerce roots :: [AbsDir])

  withAsync (liftBase (doFind `finally` atomically (closeTMQueue results))) $ \searchAsync -> do
    matches <- consumeTMQueueWithEarlyTermination @m results mempty $
      \ !acc entry@MatchEntry{matchRelPath, matchOffset} -> do
        let !relPathBS = pathForEmacs $ unRelFile matchRelPath
            key :: (ShortByteString, Word)
            !key       = (relPathBS, matchOffset)
            g :: Maybe a -> m s (Maybe a)
            g = \case
              x@Just{} -> pure x
              Nothing  -> Just <$> f relPathBS entry
        M.alterF g key acc
    wait searchAsync
    pure matches

data MatchEntry = MatchEntry
  { matchAbsPath    :: !AbsFile
  , matchRelPath    :: !RelFile
  , matchLineNum    :: !Word
  , matchColumnNum  :: !Word
  , -- | What comes before the matched text on the relevant line.
    -- Contains no newlines or non-printable characters.
    matchLinePrefix :: !BS.ByteString
  , -- | The text that was matched. May contain newlines since we
    -- support multiline matches.
    matchLineStr    :: !BS.ByteString
  , -- | What comes after the matched text on the relevant line.
    -- Contains no newlines or non-printable characters.
    matchLineSuffix :: !BS.ByteString
  , -- | Start of where 'matchlineStr' actually starts mathing in the input.
    -- Measured in byte index, not character index.
    matchOffset     :: !Word
  }
  deriving (Eq, Show, Generic)
  deriving Pretty via PPGeneric MatchEntry

data MatchState = MatchState
  { msPos     :: !Word
  , msLine    :: !Word
  , msCol     :: !Word
  , msMatches :: [(MatchOffset, MatchLength)]
  , msResult  :: [MatchEntry]
  }

isLineDelimiter :: Word8 -> Bool
isLineDelimiter w = w < 0x20

makeMatches
  :: MonadThrow m
  => AbsDir  -- ^ Directory where recursive search was initiated
  -> AbsFile -- ^ Matched file under the directory
  -> [(MatchOffset, MatchLength)]
  -> BSL.ByteString
  -> m [MatchEntry]
makeMatches (AbsDir searchRoot) fileAbsPath'@(AbsFile fileAbsPath) ms str =
  case stripProperPrefix searchRoot fileAbsPath of
    Nothing -> throwM $ mkUserError "emacsGrepRec" $
      "Internal error: findRec produced wrong root for path" <+> pretty (pathToText fileAbsPath) Semi.<>
      ". The root is" <+> pretty (pathToText searchRoot)
    Just relPath -> do
      let final = BSL.foldl' (\acc c -> accumulateMatch $ bumpPos acc $ unsafeChr $ fromIntegral c) initState str
      pure $ msResult final
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
          | msPos' == fi offset
          = s { msMatches = remainingMatches', msResult = newEntries ++ msResult }
          | otherwise
          = s
          where
            msPos' :: Int64
            !msPos' = fi msPos
            (currentMatches, remainingMatches') =
              first (map snd) $
                L.span ((== msPos') . fi . fst) remainingMatches
            newEntries =
              [ MatchEntry
                 { matchAbsPath    = fileAbsPath'
                 , matchRelPath    = RelFile relPath
                 , matchLineNum    = msLine
                 , matchColumnNum  = msCol
                 , matchLinePrefix = BSL.toStrict prefix
                 , matchLineStr    = BSL.toStrict matched
                 , matchLineSuffix = BSL.toStrict suffix
                 , matchOffset     = msPos + 1 -- Emacs counts offsets from 1
                 }
              | len <- currentMatches
              , let (before, after)  = BSL.splitAt msPos' str
                    prefix           = takeUtfLineEnd before
                    (matched, rest') = BSL.splitAt (fi len) after
                    suffix           = takeUtfLineFront rest'
              ]
            fi :: Integral a => a -> Int64
            fi = fromIntegral

data TakeEndState = TakeEndState
  { tesIdx   :: !Int
  , tesState :: !UnicodeBackwardValidationState
  }
  deriving (Generic)
  deriving Pretty via PPGeneric TakeEndState

takeUtfLineEnd :: BSL.ByteString -> BSL.ByteString
takeUtfLineEnd str = BSL.takeEnd (min 1000 (fromIntegral startIdx)) str
  where
    -- todo: remove fold here, we don't want to traverse whole string here, which may be really long.
    TakeEndState startIdx _ = BSL.foldr' feedOneBack (TakeEndState 0 Start) str

    feedOneBack :: Word8 -> TakeEndState -> TakeEndState
    feedOneBack !w !orig@TakeEndState{tesIdx, tesState} =
      case feedPrevByte w tesState of
        InvalidUtf8           -> orig { tesState = InvalidUtf8 }
        Found 1
          | isLineDelimiter w -> orig { tesState = InvalidUtf8 }
        s@(Found len)         ->
          TakeEndState
            { tesIdx   = tesIdx + len
            , tesState = s
            }
        s                     -> orig { tesState = s }

data TakeFrontState = TakeFrontState
  { tfsIdx   :: !Int
  , tfsState :: !UnicodeForwardValidationState
  }
  deriving (Generic)
  deriving Pretty via PPGeneric TakeFrontState

takeUtfLineFront :: BSL.ByteString -> BSL.ByteString
takeUtfLineFront str = BSL.take (min 1000 (fromIntegral endIdx)) str
  where
    -- todo: remove fold here, we don't want to traverse whole string here, which may be really long.
    TakeFrontState endIdx _ = BSL.foldl' (flip feedOneFront) (TakeFrontState 0 ForwardStart) str

    feedOneFront :: Word8 -> TakeFrontState -> TakeFrontState
    feedOneFront !w !orig@TakeFrontState{tfsIdx, tfsState} =
      case feedNextByte w tfsState of
        ForwardInvalidUtf8    -> orig { tfsState = ForwardInvalidUtf8 }
        ForwardFound 1
          | isLineDelimiter w -> orig { tfsState = ForwardInvalidUtf8 }
        s@(ForwardFound len)  -> TakeFrontState
          { tfsIdx   = tfsIdx + len
          , tfsState = s
          }
        s                     -> orig { tfsState = s }
