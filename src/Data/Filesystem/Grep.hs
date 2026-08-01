-- |
-- Module:     Data.Filesystem.Grep
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE QuasiQuotes           #-}

module Data.Filesystem.Grep
  ( grep
  , AnyFilesMatched(..)
  , MatchEntry(..)
  ) where

import Codec.Compression.GZip qualified as GZIP
import Control.Concurrent.Async.Lifted.Safe
import Control.Concurrent.BoolFlag qualified as BoolFlag
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Data.ByteString qualified as BS
import Data.ByteString.Char8.Ext qualified as C8
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Unsafe qualified as BSU
import Data.Coerce
import Data.Foldable
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Ord
import Data.Semigroup as Semi
import Data.Text (Text)
import Data.Word (Word8, Word32)
import Data.Word.Ext
import Foreign.C.Types (CChar)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import GHC.Base (unsafeChr)
import Prettyprinter
import Prettyprinter.Generics
import System.Directory.OsPath.Types
import System.IO.MMap (mmapWithFilePtr, Mode(ReadOnly))
import System.OsPath
import System.OsPath.Ext

import Emacs.Module
import Emacs.Module.Errors

import Data.Emacs.Path
import Data.Filesystem.Find
import Data.Ignores
import Data.Regex
import Data.UnicodeUtils
import Emacs.EarlyTermination

-- NB order of constructors is significant.
data AnyFilesMatched = NoFilesMatched | SomeFilesMatched
  deriving (Eq, Ord, Show, Generic)
  deriving Pretty via PPGeneric AnyFilesMatched

anyMatchedToBool :: AnyFilesMatched -> Bool
anyMatchedToBool = \case
  NoFilesMatched   -> False
  SomeFilesMatched -> True

boolToAnyMatched :: Bool -> AnyFilesMatched
boolToAnyMatched = \case
  False -> NoFilesMatched
  True  -> SomeFilesMatched

grep
  :: forall m s v a. (MonadEmacs m v, forall ss. MonadThrow (m ss), MonadBaseControl IO (m s), Forall (Pure (m s)))
  => [OsPath]
  -> BS.ByteString
  -> [Text]
  -> Bool
  -> Ignores
  -> Ignores
  -> (ShortByteString -> MatchEntry -> m s a)
  -> m s (Map (ShortByteString, Word) a, AnyFilesMatched)
grep roots regexp globsToFind ignoreCase fileIgnores dirIgnores f = do
  let flags = flagUnicode <> flagMultiline <> if ignoreCase then flagCaseInsensitive else mempty

  regexp' <- compileReWithOpts flags regexp

  extsToFindRE <- fileGlobsToRegex globsToFind

  let
    searchUncompressed :: AbsDir -> AbsFile -> BS.ByteString -> Ptr b -> Int -> IO (Maybe ([MatchEntry], AnyFilesMatched))
    searchUncompressed root absPath contents ptr' size = do
      matches <- reAllUtf8PtrMatches regexp' (castPtr ptr') size
      case matches of
        ReversedList [] -> pure $ Just ([], SomeFilesMatched)
        ReversedList ms -> Just . (, SomeFilesMatched) <$> makeMatches root absPath ms (castPtr ptr') size contents

    searchFile :: AbsDir -> AbsFile -> Relative OsPath -> Basename OsPath -> IO (Maybe ([MatchEntry], AnyFilesMatched))
    searchFile root absPath _ (Basename basePath)
      | isIgnoredFile fileIgnores absPath = pure Nothing
      | hasExtension (unAbsFile absPath)
      , reSetMatchesOsPath extsToFindRE basePath = do
          absPath' <- decodeUtf $ unAbsFile absPath
          mmapWithFilePtr
            absPath'
            ReadOnly
            Nothing -- map whole file
            $ \(ptr, size) -> do
              contents <- BSU.unsafePackCStringLen (castPtr ptr, size)
              let isGzip =
                    takeExtension (unAbsFile absPath) == [osp|.gz|] &&
                      "\x1f\x8b\x08" `BS.isPrefixOf` contents
              if size > 7 && isGzip
              then do
                !uncompressedSize <- getWord32LE (plusPtr (castPtr ptr :: Ptr Word32) (size - 4))
                if uncompressedSize < 2 * 1024 * 1024 * 1024
                then do
                  let params    = GZIP.defaultDecompressParams
                        { GZIP.decompressBufferSize = fromIntegral uncompressedSize }
                      contents' =
                        BSL.toStrict $
                          GZIP.decompressWith params $
                            BSL.fromStrict contents
                  res <- try $ evaluate $ rnf contents'
                  case res of
                    Left (_ :: GZIP.DecompressError) ->
                      searchUncompressed root absPath contents ptr size
                    Right !() ->
                      BSU.unsafeUseAsCStringLen contents' $ \(ptr', size') ->
                        searchUncompressed root absPath contents' ptr' size'
                else
                  searchUncompressed root absPath contents ptr size
              else searchUncompressed root absPath contents ptr size
      | otherwise = pure Nothing

  (results :: TMQueue MatchEntry) <- liftBase newTMQueueIO

  anyMatched <- liftBase $ BoolFlag.new $ anyMatchedToBool NoFilesMatched

  let collect :: ([MatchEntry], AnyFilesMatched) -> IO ()
      collect (matches, haveFileMatches) = do
        _ <- BoolFlag.or anyMatched (anyMatchedToBool haveFileMatches)
        traverse_ (atomically . writeTMQueue results) matches

      doFind :: IO ()
      doFind =
        traverse_ collect =<<
          findRec
            FollowSymlinks
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
    anyMatched' <- liftBase $ BoolFlag.get anyMatched
    pure (matches, boolToAnyMatched anyMatched')

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
  , msMatches :: ![Match]
  , msResult  :: ![MatchEntry]
  }

isLineDelimiter :: Word8 -> Bool
isLineDelimiter w = w < 0x20 && w /= 9

makeMatches
  :: AbsDir  -- ^ Directory where recursive search was initiated
  -> AbsFile -- ^ Matched file under the directory
  -> [Match]
  -> Ptr CChar
  -> Int
  -> BS.ByteString
  -> IO [MatchEntry]
makeMatches !(AbsDir searchRoot) !fileAbsPath'@(AbsFile fileAbsPath) !ms !ptr !size !str =
  case stripProperPrefix searchRoot fileAbsPath of
    Nothing -> throwM $ mkUserError "emacsGrepRec" $
      "Internal error: findRec produced wrong root for path" <+> pretty (pathToText fileAbsPath) Semi.<>
      ". The root is" <+> pretty (pathToText searchRoot)
    Just relPath -> do
      final <- C8.foldCharsM
        ptr
        size
        initState
        (\ !acc !c ->
          accumulateMatch $ bumpPos acc $ unsafeChr $ fromIntegral c)
      pure $ msResult final
      where
        initState = MatchState
          { msPos     = 0
          , msLine    = 1 -- Emacs starts to count lines from 1.
          , msCol     = 0
          , msMatches = L.sortBy (comparing matchStart) ms
          , msResult  = []
          }
        bumpPos :: MatchState -> Char -> MatchState
        bumpPos s@MatchState{msMatches = []} _    = s
        bumpPos s@MatchState{msPos}          '\r' = s { msPos = msPos + 1 }
        bumpPos s@MatchState{msPos, msLine}  '\n' = s { msPos = msPos + 1, msLine = msLine + 1, msCol = 0 }
        bumpPos s@MatchState{msPos, msCol}   _    = s { msPos = msPos + 1, msCol  = msCol + 1 }

        accumulateMatch :: MatchState -> IO MatchState
        accumulateMatch s@MatchState{msMatches = []} = pure s
        accumulateMatch s@MatchState{msPos, msLine, msCol, msMatches = remainingMatches@(m : _), msResult}
          | msPos == fromIntegral (matchStart m)
          = do
            -- Must evaluate MatchStates completely before this function finishes or we’ll
            -- be in trouble since mmapped file goes out of scope after we finish.
            newEntries' <- traverse evaluate newEntries
            pure $ s { msMatches = remainingMatches', msResult = newEntries' ++ msResult }
          | otherwise
          = pure s
          where
            currentMatches, remainingMatches' :: [Match]
            (currentMatches, remainingMatches') =
              L.span ((== msPos) . fromIntegral . matchStart) remainingMatches
            newEntries :: [MatchEntry]
            newEntries =
              [ MatchEntry
                  { matchAbsPath    = fileAbsPath'
                  , matchRelPath    = RelFile relPath
                  , matchLineNum    = msLine
                  , matchColumnNum  = msCol
                  , matchOffset     = msPos
                  -- It's crucial to copy since bytestring contents together with
                  -- the passed pointer will soon go out of scope since we want
                  -- to free mmapped file as soon as possible.
                  , matchLinePrefix = BS.copy prefix
                  , matchLineStr    = BS.copy matched
                  , matchLineSuffix = BS.copy suffix
                  }
              | currMatch <- currentMatches
              , let len              = matchEnd currMatch - matchStart currMatch
                    (before, after)  = C8.splitAt (fromIntegral msPos) str
                    prefix           = takeUtfLineEnd before
                    (matched, rest') = C8.splitAt len after
                    suffix           = takeUtfLineFront rest'
              ]

data TakeEndState = TakeEndState
  { tesIdx   :: !Int
  , tesState :: !UnicodeBackwardValidationState
  }
  deriving (Generic)
  deriving Pretty via PPGeneric TakeEndState

takeUtfLineEnd :: BS.ByteString -> BS.ByteString
takeUtfLineEnd str = BS.takeEnd (min 1000 startIdx) str
  where
    loop :: Int -> TakeEndState -> TakeEndState
    loop 0   !tes = tes
    loop idx !tes = case feedOneBack (BSU.unsafeIndex str idx') tes of
      Nothing   -> tes
      Just tes' -> loop idx' tes'
      where
        !idx' = idx - 1

    TakeEndState startIdx _ = loop (BS.length str) (TakeEndState 0 Start)

    feedOneBack :: Word8 -> TakeEndState -> Maybe TakeEndState
    feedOneBack !w !orig@TakeEndState{tesIdx, tesState} =
      case feedPrevByte w tesState of
        InvalidUtf8           -> Nothing
        Found 1
          | isLineDelimiter w -> Nothing
        s@(Found len)         -> Just TakeEndState
          { tesIdx   = tesIdx + len
          , tesState = s
          }
        s                     -> Just orig { tesState = s }

data TakeFrontState = TakeFrontState
  { tfsIdx   :: !Int
  , tfsState :: !UnicodeForwardValidationState
  }
  deriving (Generic)
  deriving Pretty via PPGeneric TakeFrontState

takeUtfLineFront :: BS.ByteString -> BS.ByteString
takeUtfLineFront str = BSU.unsafeTake (min 1000 endIdx) str
  where
    loop :: Int -> TakeFrontState -> TakeFrontState
    loop idx !tfs
      | idx == BS.length str
      = tfs
      | otherwise
      = case feedOneFront (BSU.unsafeIndex str idx) tfs of
        Nothing   -> tfs
        Just tes' -> loop (idx + 1) tes'

    TakeFrontState endIdx _ = loop 0 (TakeFrontState 0 ForwardStart)

    feedOneFront :: Word8 -> TakeFrontState -> Maybe TakeFrontState
    feedOneFront !w !orig@TakeFrontState{tfsIdx, tfsState} =
      case feedNextByte w tfsState of
        ForwardInvalidUtf8    -> Nothing
        ForwardFound 1
          | isLineDelimiter w -> Nothing
        s@(ForwardFound len)  -> Just TakeFrontState
          { tfsIdx   = tfsIdx + len
          , tfsState = s
          }
        s                     -> Just orig { tfsState = s }
