-- | Reorder out of order Rtp packets and detect gaps in the sequence numbers.
{-# LANGUAGE BangPatterns #-}
module Data.Conduit.Audio.Reorder (reorder)
where

import           Control.Monad.IO.Class
import           Data.Conduit
import qualified Data.Set                                   as Set
import           Text.Printf
import           Data.Conduit.Audio.RtpSource

data ReorderState a =
  ReorderState {
    rsEvents     :: !(Set.Set (SequenceOf a)),
    rsMinBufEvts :: !Int,
    rsMaxBufEvts :: !Int,
    rsNext       :: !SeqNum
  }

reorder
  :: (MonadIO m, Show a)
  => Conduit (RtpEvent a) m (RtpEvent a)
reorder = reorderIdle

reorderIdle
  :: (MonadIO m, Show a)
  => Conduit (RtpEvent a) m (RtpEvent a)
reorderIdle = do
  !ib <- await
  case ib of
    Nothing -> liftIO $ putStrLn "Reorder-Idle: Done."
    (Just (OutOfBand (BeginSession si))) -> do
      liftIO $ putStrLn "Reorder-Idle: Starting new session."
      reorderActive (emptyReorderState si)
    (Just msg) -> do
      liftIO $ putStrLn ("Reorder-Idle: Unexpected message: " ++ show msg)
      return ()

reorderActive
  :: (MonadIO m, Show a)
  => ReorderState a
  -> Conduit (RtpEvent a) m (RtpEvent a)
reorderActive rs@ReorderState{..} = do
  !ib <- await
  case ib of
    Nothing -> liftIO $ putStrLn "Reorder: Done."
    (Just m@(OutOfBand _)) -> do
      liftIO $ putStrLn "Reorder Re-Starting session."
      leftover m
      reorderIdle
    (Just (InBand (SequenceOf !gapSeq Gap))) -> do
      liftIO $ putStrLn "Reorder Incoming Gap."
      reorderActive (rs { rsNext = gapSeq
                   , rsEvents = Set.filter ((<= gapSeq) . position) rsEvents })
    (Just (InBand (SequenceOf !seqNum !(NoGap e)))) ->
      if (seqNum < rsNext) then
        do liftIO $
             printf "Reorder (seqNum < rsNext): rsNext = %d, seqNum: %d, dropping: %s.\n"
             (unSeqNum rsNext) (unSeqNum seqNum) (show e)
           reorderActive rs
      else
        let
          !es = Set.insert (SequenceOf seqNum e) rsEvents
          !eventCount = Set.size es
        in
          if eventCount < rsMinBufEvts
          then reorderActive  (rs {rsEvents = es})
          else
            do (!nextSeq', !es') <- propagate rsNext 1 es
               let !eventCount' = Set.size es'
               if eventCount' < rsMaxBufEvts
                 then reorderActive (rs {rsEvents = es', rsNext = nextSeq'})
                 else
                 do (!nextSeq'', !es'') <- propagateWithGap
                                              rsNext
                                              (eventCount' - rsMinBufEvts) es'
                    reorderActive (rs {rsEvents = es'', rsNext = nextSeq''})

emptyReorderState :: SessionInfo -> ReorderState a
emptyReorderState SessionInfo{..} =
  -- TODO make jitter buffer dimensions configurable
  ReorderState Set.empty 5 25 siStartSeq

propagateWithGap
  :: (MonadIO m)
  => SeqNum
  -> Int
  -> Set.Set (SequenceOf t)
  -> ConduitM i (RtpEvent t) m (SeqNum, Set.Set (SequenceOf t))
propagateWithGap !seqNumExpected !n !es = do
  let ((SequenceOf !s body), !es') = Set.deleteFindMin es
  liftIO $
    printf "Reorder Gap detected: seqNumActual = %d,  seqNumExpected = %d.\n"
           (unSeqNum s) (unSeqNum seqNumExpected)
  yieldInbandGap s
  yieldInband s body
  propagate s (n - 1) es'

propagate
  :: (Monad m)
  => SeqNum
  -> Int
  -> Set.Set (SequenceOf t)
  -> ConduitM i (RtpEvent t) m (SeqNum, Set.Set (SequenceOf t))
propagate !s 0 !es = return (s, es)
propagate !s !n !es =
  case Set.deleteFindMin es of
    ((SequenceOf !s' body), !es') | s' == s ->
      yieldInband s' body >> propagate (s+1) (n-1) es'
    _ ->
      return (s, es)
