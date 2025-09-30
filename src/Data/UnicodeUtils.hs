-- |
-- Module:     Data.UnicodeUtils
-- Copyright:  (c) Sergey Vinokurov 2025
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE DerivingVia      #-}
{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE OrPatterns       #-}

module Data.UnicodeUtils
  ( utf8LengthByLeader
  , wchr2
  , wchr3
  , wchr4
  -- , unsafeReverseIterBack
  , UnicodeBackwardValidationState(..)
  , feedPrevByte
  , UnicodeForwardValidationState(..)
  , feedNextByte
  ) where

import GHC.Exts (isTrue#)
import GHC.Int
import GHC.Prim
import GHC.Word
import Prettyprinter.Generics

{-# INLINE utf8LengthByLeader #-}
utf8LengthByLeader :: Word8 -> Int
utf8LengthByLeader (W8# w) = I# (c# `xorI#` (c# <=# 0#))
  where
    c# = word2Int# (clz8# (word8ToWord# (notWord8# w)))

{-# INLINE wchr2 #-}
wchr2 :: Word8 -> Word8 -> Int#
wchr2 (W8# x1#) (W8# x2#) =
  z1# +# z2#
  where
    !y1# = word2Int# (word8ToWord# x1#)
    !y2# = word2Int# (word8ToWord# x2#)
    !z1# = uncheckedIShiftL# (y1# -# 0xC0#) 6#
    !z2# = y2# -# 0x80#

{-# INLINE wchr3 #-}
wchr3 :: Word8 -> Word8 -> Word8 -> Int#
wchr3 (W8# x1#) (W8# x2#) (W8# x3#) =
  z1# +# z2# +# z3#
  where
    !y1# = word2Int# (word8ToWord# x1#)
    !y2# = word2Int# (word8ToWord# x2#)
    !y3# = word2Int# (word8ToWord# x3#)
    !z1# = uncheckedIShiftL# (y1# -# 0xE0#) 12#
    !z2# = uncheckedIShiftL# (y2# -# 0x80#) 6#
    !z3# = y3# -# 0x80#

{-# INLINE wchr4 #-}
wchr4 :: Word8 -> Word8 -> Word8 -> Word8 -> Int#
wchr4 (W8# x1#) (W8# x2#) (W8# x3#) (W8# x4#) =
  z1# +# z2# +# z3# +# z4#
  where
    !y1# = word2Int# (word8ToWord# x1#)
    !y2# = word2Int# (word8ToWord# x2#)
    !y3# = word2Int# (word8ToWord# x3#)
    !y4# = word2Int# (word8ToWord# x4#)
    !z1# = uncheckedIShiftL# (y1# -# 0xF0#) 18#
    !z2# = uncheckedIShiftL# (y2# -# 0x80#) 12#
    !z3# = uncheckedIShiftL# (y3# -# 0x80#) 6#
    !z4# = y4# -# 0x80#

data UnicodeBackwardValidationState
  = Start
  | Seen1
  | Seen2
  | Seen3
  | Found !Int
  | InvalidUtf8
  deriving (Generic)
  deriving Pretty via PPGeneric UnicodeBackwardValidationState

feedPrevByte :: Word8 -> UnicodeBackwardValidationState -> UnicodeBackwardValidationState
feedPrevByte !(W8# m#) = \case
  (Start; Found{}) ->
    if isAsciiChar m#
    then Found 1
    else Seen1
  Seen1            ->
    if isInvalidUtf8MidChar m#
    then Seen2
    else Found 2
  Seen2            ->
    if isInvalidUtf8MidChar m#
    then Seen3
    else Found 3
  Seen3            ->
    if isInvalidUtf8MidChar m#
    then InvalidUtf8
    else Found 4
  InvalidUtf8      -> InvalidUtf8

data UnicodeForwardValidationState
  = ForwardStart
  | ForwardSkip !Int !Int
  | ForwardFound !Int
  | ForwardInvalidUtf8
  deriving (Generic)
  deriving Pretty via PPGeneric UnicodeForwardValidationState

feedNextByte :: Word8 -> UnicodeForwardValidationState -> UnicodeForwardValidationState
feedNextByte !m@(W8# m#) = \case
  (ForwardStart; ForwardFound{}) ->
    case utf8LengthByLeader m of
      1 -> ForwardFound 1
      n -> ForwardSkip (n - 1) n
  ForwardSkip 1 k                ->
    if isValidUtf8MidChar m#
    then ForwardFound k
    else ForwardInvalidUtf8
  ForwardSkip n k                ->
    if isValidUtf8MidChar m#
    then ForwardSkip (n - 1) k
    else ForwardInvalidUtf8
  ForwardInvalidUtf8             -> ForwardInvalidUtf8

isAsciiChar :: Word8# -> Bool
isAsciiChar w# = isTrue# (w# `ltWord8#` 0x80#Word8)

isInvalidUtf8MidChar :: Word8# -> Bool
isInvalidUtf8MidChar w# = isTrue# (w# `ltWord8#` 0xC0#Word8)

isValidUtf8MidChar :: Word8# -> Bool
isValidUtf8MidChar w# = isTrue# (w# `gtWord8#` 0x7F#Word8)
