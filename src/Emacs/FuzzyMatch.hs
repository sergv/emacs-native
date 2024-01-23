----------------------------------------------------------------------------
-- |
-- Module      :  Emacs.FuzzyMatch
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies          #-}

module Emacs.FuzzyMatch (initialise) where

import Control.Concurrent
import Control.Concurrent.Async.Lifted.Safe
import Control.Concurrent.Counter.Lifted.IO qualified as Counter
import Control.DeepSeq
import Control.LensBlaze
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.ST.Strict
import Control.Monad.Trans.Control
import Data.Foldable
import Data.Int
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Primitive.PrimArray
import Data.Primitive.Types
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Ext qualified as VExt
import Data.Vector.PredefinedSorts
import Data.Vector.Primitive qualified as P
import Data.Vector.Primitive.Mutable qualified as PM
import GHC.IO (unsafeIOToST)

import Data.Emacs.Module.Doc qualified as Doc
import Data.FuzzyMatch.SortKey
import Emacs.EarlyTermination
import Emacs.Module
import Emacs.Module.Assert (WithCallStack)
import Emacs.Module.Monad qualified as Emacs

import Data.FuzzyMatch

initialise
  :: WithCallStack
  => Emacs.EmacsM s ()
initialise = do
  bindFunction "haskell-native-score-matches" =<<
    makeFunction scoreMatches scoreMatchesDoc
  bindFunction "haskell-native-score-single-match" =<<
    makeFunction scoreSingleMatch scoreSingleMatchDoc

scoreMatchesDoc :: Doc.Doc
scoreMatchesDoc =
  "Given a query string and a list of strings to match against, \
  \sort the strings according to score of fuzzy matching them against the query."

extractSeps
  :: (MonadEmacs m v, Prim (v s), MonadIO (m s), PM.PrimState (m s) ~ RealWorld)
  => v s -> m s (PrimArray Int32)
extractSeps !xs = do
  ys <- extractVectorMutableWith (fmap fromIntegral . extractInt) xs
  liftIO $ stToIO $ sortInt32 ys
  fmap VExt.primVectorToPrimArray $ P.unsafeFreeze ys

scoreMatches
  :: forall m v s.
     ( WithCallStack
     , MonadEmacs m v
     , MonadIO (m s)
     , MonadThrow (m s)
     , MonadBaseControl IO (m s)
     , Forall (Pure (m s))
     , forall ss. MonadThrow (m ss)
     , NFData (v s)
     , Prim (v s)
     , PM.PrimState (m s) ~ RealWorld
     )
  => EmacsFunction ('S ('S ('S 'Z))) 'Z 'False m v s
scoreMatches (R seps (R needle (R haystacks Stop))) = do
  seps'   <- extractSeps seps
  needle' <- extractText needle

  (haystacks' :: V.Vector (Text, v s)) <- extractVectorWith (\str -> (, str) <$> extractText str) haystacks

  -- Will rethrow EarlyTermination if user aborted.
  (matches :: P.Vector SortKey) <- runWithEarlyTermination $ do
    let chunk :: Int
        !chunk = 256
        totalHaystacks :: Int
        !totalHaystacks = V.length haystacks'
        needleSegments :: NonEmpty Text
        needleSegments = splitNeedle needle'

    jobs    <- getNumCapabilities
    jobSync <- Counter.new (jobs * chunk)

    (scores :: PM.MVector RealWorld SortKey) <- PM.new totalHaystacks

    let processOne :: forall ss. ReusableState ss -> Text -> Int -> ST ss SortKey
        processOne !store !haystack !n = do
          let haystackLen :: Int
              !haystackLen = T.length haystack
          !match <-
            fromMaybe noMatch <$>
              fuzzyMatch'
                store
                (computeHeatmap store haystack haystackLen seps')
                needleSegments
                haystack
          pure $! mkSortKey (fi32 (mScore match)) (fromIntegral haystackLen) (fromIntegral n)

        processChunk :: forall ss. ReusableState ss -> Int -> Int -> ST ss ()
        processChunk !store !start !end =
          loopM start end $ \ !n -> do
            let !haystack = fst $ haystacks' `V.unsafeIndex` n
            unsafeIOToST . PM.unsafeWrite scores n =<< processOne store haystack n

        processChunks :: forall ss. Int -> ST ss ()
        processChunks !k = do
          store <- mkReusableState (T.length needle')

          let go :: Int -> ST ss ()
              go !start
                | start < totalHaystacks
                = do
                  processChunk store start (min totalHaystacks (start + chunk))
                  go =<< unsafeIOToST (Counter.add jobSync chunk)
                | otherwise
                = pure ()
          let !initStart = chunk * k
          go initStart

    traverse_ wait =<< traverse (async . stToIO . processChunks) [0..jobs - 1]

    stToIO $ sortSortKeyPar scores
    P.unsafeFreeze scores

  nilVal <- nil

  let mkListLoop :: Int -> v s -> m s (v s)
      mkListLoop 0 res = pure res
      mkListLoop i res = do
        let !j = i - 1
            idx :: Int
            !idx = fromIntegral $ view idxL $ matches `P.unsafeIndex` j
        mkListLoop j =<< cons (snd $ haystacks' `V.unsafeIndex` idx) res

  mkListLoop (P.length matches) nilVal

{-# INLINE loopM #-}
loopM :: Applicative m => Int -> Int -> (Int -> m ()) -> m ()
loopM !from !to action = go from
  where
    go !n
      | n == to
      = pure ()
      | otherwise
      = action n *> go (n + 1)

{-# INLINE fi32 #-}
fi32 :: Integral a => a -> Int32
fi32 = fromIntegral

scoreSingleMatchDoc :: Doc.Doc
scoreSingleMatchDoc =
  "Fuzzy match a single string against another. Returns match score and \
  \positions where the match occured."

scoreSingleMatch
  :: forall m v s.
     ( WithCallStack
     , MonadEmacs m v
     , MonadIO (m s)
     , MonadThrow (m s)
     , MonadBaseControl IO (m s)
     , Forall (Pure (m s))
     , Prim (v s)
     , PM.PrimState (m s) ~ RealWorld
     )
  => EmacsFunction ('S ('S ('S 'Z))) 'Z 'False m v s
scoreSingleMatch (R seps (R needle (R haystack Stop))) = do
  seps'     <- extractSeps seps
  needle'   <- extractText needle
  haystack' <- extractText haystack
  let !Match{mScore, mPositions} = runST $ do
        store <- mkReusableState (T.length needle')
        fromMaybe noMatch <$>
          fuzzyMatch'
            store
            (computeHeatmap store haystack' (T.length haystack') seps')
            (splitNeedle needle')
            haystack'
  score     <- makeInt $ fromIntegral mScore
  positions <- makeList =<< traverse (makeInt . fromIntegral . unStrCharIdx) mPositions
  cons score positions

noMatch :: Match
noMatch = Match
  { mScore     = (-1000000)
  , mPositions = StrCharIdx (-1) :| []
  }

