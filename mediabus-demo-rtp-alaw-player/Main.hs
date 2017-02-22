module Main where

import           Control.Monad.State
import           Data.Time.Clock
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent                ( threadDelay )
import           Control.Parallel.Strategies       ( NFData, rdeepseq, using
                                                   , withStrategy )
import qualified Data.Vector.Storable              as V
import           GHC.TypeLits

import           Conduit
import           Data.MediaBus
import           Data.MediaBus.Internal.Conduit
import           Data.MediaBus.Internal.Series
import qualified Data.MediaBus.Rtp.Packet          as Rtp
import           Data.Word
import           Control.Lens
import           Control.Monad
import           Data.Proxy
import qualified Data.MediaBus.Internal.RingBuffer as Ring
import           Data.Maybe
import           Debug.Trace
import           Text.Printf

{- Send test data with:
#!/bin/bash

PORT=${1?port missing}
MY_IP=${2?host ip missing}
FNAME=${3:-28797-04.ogg}
FILE=$(realpath $(dirname ${0})/$FNAME)

gst-launch-1.0  uridecodebin uri=file://$FILE ! audioconvert ! audioresample !  audio/x-raw,format=S16LE,rate=8000,channels=1 ! alawenc ! rtppcmapay pt=8 mtu=172 min-ptime=10000000 max-ptime=200000000  ptime-multiple=5000000 ! udpsink host=$MY_IP port=$PORT
-}
main :: IO ()
main = do
    let ptime = 10 / 1000
        ptimeTicks :: DemoTicks
        ptimeTicks = nominalDiffTime # ptime
        ringCapacity = 10
        silence = MkSampleBuffer (V.replicate (fromIntegral (_ticks ptimeTicks)) -- TODO extract type class for silence
                                              (MkS16 0))
    ringRef <- newTVarIO (Ring.newRingBuffer ringCapacity `using` rdeepseq)
    void $
        concurrently (runConduitRes (udpDatagramSource useUtcClock
                                                       10000
                                                       "127.0.0.1" .|
                                         rtpSource .|
                                         rtpPayloadDemux [ ( 8
                                                           , alawPayloadHandler
                                                           )
                                                         ]
                                                         mempty .|
                                         transcodeStreamC .|
                                         resample8to16kHz (MkS16 0 :: S16 8000) .|
                                         convertTicksC at8kHzU32 at16kHzU64 .|
                                         annotateTypeC _receiveRtpFromUDPStreamType
                                                       (reorderFramesBySeqNumC ringCapacity) .|
                                         repacketizeC ptime .|
                                         -- dbgShowC 1 "" .|
                                         mapInput (fromMaybe silence .
                                                       preview payload)
                                                  (const Nothing)
                                                  (tvarRingBufferSink ringRef)))
                     (runConduitRes (tvarRingBufferSource (0.5 * ptime *
                                                               fromIntegral ringCapacity)
                                                          silence
                                                          ringRef
                                         -- .| dbgShowC 1 ""
                                         .| annotateTypeSink _receiveRtpFromUDPStreamType
                                                             streamDebugPlaybackSink))

--  TODO create a gap detection mechanism, a simple stateful conduit that knows the next timestamp
tvarRingBufferSink :: MonadIO m => TVar (Ring.RingBuffer a) -> Sink a m ()
tvarRingBufferSink ringRef =
    awaitForever (liftIO .
                      atomically .
                          modifyTVar ringRef . withStrategy rdeepseq . Ring.push)

tvarRingBufferSource :: (NFData c, Num i, HasDuration c, MonadIO m, KnownNat r, Integral t, Integral s)
                     => NominalDiffTime
                     -> c
                     -> TVar (Ring.RingBuffer c)
                     -> Source m (Stream i s (Ticks r t) c)
tvarRingBufferSource pollInterval silence ringRef =
    let pollIntervalMicros :: Ticks 1000000 Int
        pollIntervalMicros = nominalDiffTime # pollInterval
        -- TODO: totally recreate the sequence numbers and timestamps!
        popFromRingPollInterval duration acc ring underflow =
            if duration >= pollInterval
            then (reverse acc, ring)
            else let (buf, ring') = Ring.popAndSet silence ring
                     duration' = getDuration buf + duration
                     underflow' = Ring.size ring == 0
                 in
                     (if underflow' /= underflow
                      then trace (printf "*** RING UNDERFLOW after %s ***"
                                         (show duration'))
                      else id) popFromRingPollInterval
                               duration'
                               (buf : acc)
                               ring'
                               underflow'
    in
        (do
             yield (MkStream (Start (MkFrameCtx 0 0 0)))
             forever (liftIO (do
                                  threadDelay (_ticks pollIntervalMicros)
                                  atomically $ do
                                      !ring <- readTVar ringRef
                                      let (!frms, !ring') = popFromRingPollInterval 0
                                                                                    []
                                                                                    ring
                                                                                    (Ring.size ring ==
                                                                                         0)
                                              `using` rdeepseq
                                      writeTVar ringRef ring'
                                      return frms)
                          >>= Control.Monad.mapM_ (yield .
                                                       MkStream .
                                                           Next . MkFrame 0 0)))
            .| synchronizeToSeqNum 0
            .| evalStateC 0
                          (mapMC (\ !frm -> do
                                      let !frmDur = nominalDiffTime #
                                              getDuration frm
                                      !ts <- get
                                      put (ts + frmDur)
                                      return (frm & timestamp .~ ts)))

_receiveRtpFromUDPStreamType :: Proxy (Stream Rtp.RtpSsrc Rtp.RtpSeqNum DemoTicks (SampleBuffer (S16 DemoRate)))
_receiveRtpFromUDPStreamType =
    Proxy

type DemoTicks = Ticks DemoRate Word64

type DemoRate = 16000
