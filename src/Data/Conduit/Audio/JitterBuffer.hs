{-# LANGUAGE BangPatterns #-}
module Data.Conduit.Audio.JitterBuffer
  ( JitterBuffer()
  , emptyJitterBuffer
  , dejitter)
where

import           Control.Monad.IO.Class
import           Data.Conduit
import qualified Data.Set               as Set
import           Text.Printf
import           Data.Conduit.Audio.RtpPacket (SeqNum(..))
import           Data.Conduit.Audio.Event

-- | TODO rewrite to use the actual time stamps, then rewrite a UDP source that
-- sends constant rate packets, then detect missing packets and transmission
-- delays in general. Add roundtrip time statistics and jitter calculation for
-- RTCP.
data JitterBuffer a =
  JitterBuffer {
    jbEvents     :: !(Set.Set (Event a)),
    jbMinBufEvts :: !Int,
    jbMaxBufEvts :: !Int,
    jbNext       :: !SeqNum
  }

emptyJitterBuffer :: Int -> Int -> SeqNum -> JitterBuffer a
emptyJitterBuffer minEvts maxEvts =
  JitterBuffer Set.empty minEvts maxEvts

dejitter :: Show a => JitterBuffer a -> Conduit (Event a) IO (Event a)
dejitter jb@JitterBuffer{..} = do
  !ib <- await
  case ib of
    Nothing ->
      liftIO $ putStrLn "Dejitter: Done."
    (Just !e) ->
      let !seqNum = eventSeq e in
        if isDiscontinuity e then
          do liftIO $
               printf "Dejitter Discontinuity: seqNum = %d\n"
               (unSeqNum jbNext) (unSeqNum seqNum) (show e)
             yieldDiscontinuity seqNum
             dejitter (jb {jbEvents = Set.empty, jbNext = seqNum})
        else
          if (seqNum < jbNext) then
            do liftIO $
                 printf "Dejitter (seqNum < jbNext): jbNext = %d, seqNum: %d, dropping: %s\n"
                 (unSeqNum jbNext) (unSeqNum seqNum) (show e)
               dejitter jb
          else
            let
              !es = Set.insert e jbEvents
              !eventCount = Set.size es
            in
              if eventCount < jbMaxBufEvts then
                if eventCount > jbMinBufEvts then
                  do (!nextSeq', !es') <- dejitterPropagate jbNext 1 es
                     dejitter (jb {jbEvents = es', jbNext = nextSeq'})
                else
                  dejitter  (jb {jbEvents = es})
              else
                do (!nextSeq', !es') <-
                     dejitterPropagateForce (eventCount - jbMinBufEvts) es
                   dejitter (jb {jbEvents = es', jbNext = nextSeq'})

dejitterPropagateForce
  :: (Monad m)
  => Int
  -> Set.Set (Event t)
  -> ConduitM i (Event t) m (SeqNum, Set.Set (Event t))
dejitterPropagateForce !n !es =
  let (!e, !es') = Set.deleteFindMin es
  in yield e >> dejitterPropagate (eventSeq e) (n - 1) es'

dejitterPropagate
  :: (Monad m)
  => SeqNum
  -> Int
  -> Set.Set (Event t)
  -> ConduitM i (Event t) m (SeqNum, Set.Set (Event t))
dejitterPropagate !s 0 !es = return (s, es)
dejitterPropagate !s !n !es =
  case Set.deleteFindMin es of
    (!e, !es') | eventSeq e == s ->
      yield e >> dejitterPropagate (s+1) (n-1) es'
    _ ->
      return (s, es)
