-- |
-- Module:     Data.ElispSexp
-- Copyright:  (c) Sergey Vinokurov 2026
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LinearTypes       #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples     #-}
{-# LANGUAGE UnliftedDatatypes #-}

module Data.ElispSexp
  ( Sexp(..)
  , ToSexp(..)
  , renderSexp
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.Text (Text)
import Data.Text.Builder.Linear.Buffer
import Data.Text.Encoding qualified as T
import Data.Text.Ext qualified as T
import System.OsPath.Ext

import Data.Filesystem.Find.Types
import Data.Filesystem.Grep qualified as Grep

data Sexp
  = List [Sexp]
  | String !Text
  | Symbol !Text
  | Keyword !Text
  | Number !Int
  deriving (Show)

data DelimiterRequirement = NeedsDelimiters | SelfContained

data BufDelim where
  BufDelim :: {-# UNPACK #-} Buffer %1 -> DelimiterRequirement -> BufDelim

renderSexp :: Sexp -> Text
renderSexp s = runBuffer (\b -> case go s b of BufDelim b' _ -> b')
  where
    go :: Sexp -> Buffer %1 -> BufDelim
    go sexp buf = case sexp of
      List   xs ->
        case goList xs (BufDelim (buf |>. '(') SelfContained) of
          BufDelim b _ -> BufDelim (b |>. ')') SelfContained
      String  x -> BufDelim (escapeStr (buf |>. '"') x |>. '"') SelfContained
      Symbol  x -> BufDelim (buf |> x) NeedsDelimiters
      Keyword x -> BufDelim (buf |>. ':' |> x) NeedsDelimiters
      Number  x -> BufDelim (buf |>$ x) NeedsDelimiters

    escapeStr :: Buffer %1 -> Text -> Buffer
    escapeStr b str =
      T.textFoldLinear f b str
      where
        f :: Char -> Buffer %1 -> Buffer
        f c buf = case c of
          '\0'  -> buf |># "\\0"#
          '\n'  -> buf |># "\\n"#
          '\r'  -> buf |># "\\r"#
          '\t'  -> buf |># "\\t"#
          '\\'  -> buf |># "\\\\"#
          '"'   -> buf |># "\\\""#
          other -> buf |>. other

    goList :: [Sexp] -> BufDelim %1 -> BufDelim
    goList ys (BufDelim b delim) =
      case ys of
        []     -> BufDelim b SelfContained
        z : zs -> goList' z zs b'
          where
            b' = case delim of
              SelfContained   -> b
              NeedsDelimiters -> b |>. ' '

    goList' :: Sexp -> [Sexp] -> Buffer %1 -> BufDelim
    goList' z zs b = goList zs (go z b)

-- data Ur a where
--   Ur :: a -> Ur a
--
-- type BufferWithDelimiter' = (# Buffer, Ur DelimiterRequirement #)
--
-- renderSexp' :: Sexp -> Text
-- renderSexp' s = runBuffer (\b -> case go s b of (# b', _ #) -> b')
--   where
--     go :: Sexp -> Buffer %1 -> BufferWithDelimiter'
--     go sexp buf = case sexp of
--       List    xs -> goList xs (# buf, SelfContained #)
--       String  x  -> (# buf |>. '"' |> x |>. '"', SelfContained #)
--       Symbol  x  -> (# buf |> x,                 NeedsDelimiters #)
--       Keyword x  -> (# buf |>. ':' |> x,         NeedsDelimiters #)
--       Number  x  -> (# buf |>$ x,                NeedsDelimiters #)
--
--     goList :: [Sexp] -> BufferWithDelimiter' %1 -> BufferWithDelimiter'
--     goList ys (# b, Ur delim #) =
--       case ys of
--         []     -> (# b, SelfContained #)
--         z : zs -> goList' z zs b'
--           where
--             b' = case delim of
--               SelfContained   -> b
--               NeedsDelimiters -> b |>. ' '
--
--     goList' :: Sexp -> [Sexp] -> Buffer %1 -> BufferWithDelimiter'
--     goList' z zs b = goList zs (go z b)

class ToSexp a where
  toSexp :: a -> Sexp

instance ToSexp a => ToSexp [a] where
  toSexp = List . map toSexp

instance ToSexp Word where
  toSexp = Number . fromIntegral

instance ToSexp Text where
  toSexp = String

instance ToSexp AbsFile where
  toSexp = toSexp . pathToText . unAbsFile

instance ToSexp RelFile where
  toSexp = toSexp . pathToText . unRelFile

instance ToSexp BS.ByteString where
  toSexp = String . T.decodeLatin1 . Base64.encode

instance ToSexp Grep.MatchEntry where
  toSexp
    Grep.MatchEntry
      { Grep.matchAbsPath
      , Grep.matchRelPath
      , Grep.matchLineNum
      , Grep.matchColumnNum
      , Grep.matchLinePrefix
      , Grep.matchLineStr
      , Grep.matchLineSuffix
      , Grep.matchOffset
      }
    = List
        [ Keyword "absolute-path", toSexp matchAbsPath
        , Keyword "relative-path", toSexp matchRelPath
        , Keyword "line", toSexp matchLineNum
        , Keyword "column", toSexp matchColumnNum
        , Keyword "match-prefix", toSexp matchLinePrefix
        , Keyword "match-string", toSexp matchLineStr
        , Keyword "match-suffix", toSexp matchLineSuffix
        , Keyword "byte-offset", toSexp matchOffset
        ]
