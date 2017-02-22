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
import           System.Random

import           Conduit
import           Data.MediaBus
import           Data.MediaBus.Internal.Conduit
import           Data.MediaBus.Internal.Series
import qualified Data.MediaBus.Rtp.Packet          as Rtp
import           Data.Word
import           Control.Lens
import           Data.Proxy
import qualified Data.MediaBus.Internal.RingBuffer as Ring
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
        race (runConduitRes (udpDatagramSource useUtcClock
                                                       10000
                                                       "127.0.0.1" .|
                                         rtpSource .|
                                         rtpPayloadDemux [ ( 8
                                                           , alawPayloadHandler
                                                           )
                                                         ]
                                                         mempty .|
                                         transcodeStreamC' .|
                                         resample8to16kHz' (MkS16 0 :: S16 8000) .|
                                         convertTicksC' at8kHzU32 at16kHzU64 .|
                                         annotateTypeC _receiveRtpFromUDPStreamType
                                                       (reorderFramesBySeqNumC ringCapacity) .|
                                         repacketizeC ptime .|
                                         -- dbgShowC 1 "" .|
                                         tvarRingBufferSinkTraversal payload
                                                                     ringRef))
                     (runConduitRes (tvarRingBufferSource (0.5 * ptime *
                                                               fromIntegral ringCapacity)
                                                          silence
                                                          ringRef
                                         -- .| dbgShowC 1 ""
                                         .| debugExitAfter 1000
                                         .| annotateTypeSink _receiveRtpFromUDPStreamType
                                                             streamDebugPlaybackSink))

--  TODO create a gap detection mechanism, a simple stateful conduit that knows the next timestamp
tvarRingBufferSinkTraversal :: MonadIO m
                            => Traversal' s a
                            -> TVar (Ring.RingBuffer a)
                            -> Sink s m ()
tvarRingBufferSinkTraversal trav ringRef =
    awaitForever go
  where
    go x = maybe (return ()) pushInRing (x ^? trav)
      where
        pushInRing = liftIO .
            atomically . modifyTVar ringRef . withStrategy rdeepseq . Ring.push

tvarRingBufferSource :: (NFData c, Random i, HasDuration c, MonadIO m, KnownNat r, Integral t, Integral s, Random t, Random s)
                     => NominalDiffTime
                     -> c
                     -> TVar (Ring.RingBuffer c)
                     -> Source m (Stream i s (Ticks r t) c)
tvarRingBufferSource pollInterval silence ringRef =
    evalStateC (0, 0) $ do
        setRandomStartCtx
        yieldStart
        forever (pollNextBuffers >>= mapM_ yieldNextBuffer)
  where
    setRandomStartCtx = do
        ts0 <- liftIO randomIO
        sn0 <- liftIO randomIO
        put (ts0, sn0)

    yieldStart = MkFrameCtx <$> liftIO randomIO <*> use _1 <*> use _2 >>=
        yield . MkStream . Start

    pollNextBuffers = liftIO $ do
        threadDelay (_ticks pollIntervalMicros)
        atomically $ do
            !ring <- readTVar ringRef
            let (!bufs, !ring') = popFromRingPollInterval 0
                                                          []
                                                          ring
                                                          (Ring.size ring ==
                                                               0)
                    `using` rdeepseq
            writeTVar ringRef ring'
            return bufs
      where
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

        pollIntervalMicros :: Ticks 1000000 Int
        pollIntervalMicros = nominalDiffTime # pollInterval

    yieldNextBuffer buf = do
        ts <- _1 <<+= nominalDiffTime # getDuration buf
        sn <- _2 <<+= 1
        yield $
            MkStream $
                Next $
                    MkFrame ts sn buf

_receiveRtpFromUDPStreamType :: Proxy (Stream Rtp.RtpSsrc Rtp.RtpSeqNum DemoTicks (SampleBuffer (S16 DemoRate)))
_receiveRtpFromUDPStreamType =
    Proxy

type DemoTicks = Ticks DemoRate Word64

type DemoRate = 16000
