-- |
-- Module:     Emacs.PredefinedSymbols
-- Copyright:  (c) Sergey Vinokurov 2026
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

module Emacs.PredefinedSymbols
  ( fullIgnores
  , makeEgrepMatch
  , noFilesMatched
  ) where

import Data.Emacs.Module.SymbolName (SymbolName)
import Data.Emacs.Module.SymbolName.TH

cacheSym "haskell-native--full-ignores" "fullIgnoresTH"
cacheSym "make-egrep-match" "makeEgrepMatchTH"
cacheSym "no-files-matched" "noFilesMatchedTH"

{-# INLINE fullIgnores #-}
fullIgnores :: SymbolName
fullIgnores = fullIgnoresTH

{-# INLINE makeEgrepMatch #-}
makeEgrepMatch :: SymbolName
makeEgrepMatch = makeEgrepMatchTH

{-# INLINE noFilesMatched #-}
noFilesMatched :: SymbolName
noFilesMatched = noFilesMatchedTH
