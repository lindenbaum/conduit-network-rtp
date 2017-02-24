module Main where

import           Control.Monad.State
import           Data.Time.Clock
import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.STM
import           Control.Concurrent                ( threadDelay )
import           Control.Parallel.Strategies       ( NFData, rdeepseq, using
                                                   , withStrategy )
import           Control.Monad.Trans.Control       ( MonadBaseControl(StM) )
import qualified Data.Vector.Storable              as V
import           GHC.TypeLits
import           System.Random
import           Conduit
import           Data.MediaBus
import           Data.MediaBus.Internal.Series
import qualified Data.MediaBus.Rtp.Packet          as Rtp
import           Data.Word
import           Control.Lens
import           Data.Proxy
import qualified Data.MediaBus.Internal.RingBuffer as Ring
import           Debug.Trace
import           Text.Printf
import           Data.Default
import           GHC.Generics                      ( Generic )
import           Data.Tagged
import           Data.Streaming.Network            ( HostPreference )

data Discontinous a = Missing !NominalDiffTime
                    | Got !a
    deriving (Show, Generic)

instance NFData a =>
         NFData (Discontinous a)

instance Default (Discontinous a) where
    def = Missing 1

makePrisms ''Discontinous

instance HasDuration a =>
         HasDuration (Discontinous a) where
    getDuration (Missing !d) =
        d
    getDuration (Got !x) = getDuration x

instance HasPayload a =>
         HasPayload (Discontinous a) where
    type GetPayload (Discontinous a) = GetPayload a
    type SetPayload (Discontinous a) b = Discontinous (SetPayload a b)
    payload = _Got . payload

data STicksK = STicks Nat Nat

type family STicksGetRate (s :: STicksK) :: Nat where
        STicksGetRate ('STicks r t) = r

type family STicksGetTicks (s :: STicksK) :: Nat where
        STicksGetTicks ('STicks r t) = t

type WithSTicks r t x = Tagged ('STicks r t) x

type DiscontinousWithSTicks r t x = WithSTicks r t (Discontinous x)

class SetStaticDuration s (GetStaticDuration s) ~ s =>
      HasStaticDuration s where
    type SetStaticDuration s (pt :: STicksK)
    type GetStaticDuration s :: STicksK

instance (KnownNat r, KnownNat t) =>
         HasStaticDuration (Tagged ('STicks r t) x) where
    type SetStaticDuration (Tagged ('STicks r t) x) pt = Tagged pt x
    type GetStaticDuration (Tagged ('STicks r t) x) = 'STicks r t

instance (KnownNat r, KnownNat t) =>
         HasDuration (Tagged ('STicks r t) x) where
    getDuration _ = getStaticDuration (Proxy :: Proxy (Tagged ('STicks r t) x))

toSTicksProxy :: (HasStaticDuration s, GetStaticDuration s ~ 'STicks r t)
              => proxy s
              -> Proxy ('STicks r t)
toSTicksProxy _ = Proxy

getStaticDuration :: forall proxy s r t.
                  (KnownNat r, KnownNat t, HasStaticDuration s, GetStaticDuration s ~ 'STicks r t)
                  => proxy s
                  -> NominalDiffTime
getStaticDuration px = from nominalDiffTime # ticksFromSTicks (toSTicksProxy px)

getStaticTicks :: forall proxy s r t i.
               (KnownNat r, KnownNat t, HasStaticDuration s, GetStaticDuration s ~ 'STicks r t, Integral i)
               => proxy s
               -> Ticks r i
getStaticTicks px = ticksFromSTicks (toSTicksProxy px)

getStaticRate :: forall proxy s r t.
              (KnownNat r, KnownNat t, HasStaticDuration s, GetStaticDuration s ~ 'STicks r t)
              => proxy s
              -> Integer
getStaticRate px = rateFromSTicks (toSTicksProxy px)

ticksFromSTicks :: forall proxy rate ticks i.
                (KnownNat rate, KnownNat ticks, Integral i)
                => proxy ('STicks rate ticks)
                -> Ticks rate i
ticksFromSTicks _ = MkTicks (fromIntegral (natVal (Proxy :: Proxy ticks)))

rateFromSTicks :: forall proxy rate ticks.
               (KnownNat rate, KnownNat ticks)
               => proxy ('STicks rate ticks)
               -> Integer
rateFromSTicks _ = fromIntegral (natVal (Proxy :: Proxy rate))

concealMissing :: (NFData c, Monad m)
               => (NominalDiffTime -> c)
               -> Conduit (Stream i s t (Discontinous c)) m (Stream i s t c)
concealMissing concealF =
    mapPayloadC' go
  where
    go (Got !b) = b
    go (Missing !dur) = concealF dur


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
    let !ptime = 10 / 1000
        !frameQLength = 20
        !pollIntervall = 0.5 * fromIntegral frameQLength * ptime
    (!asrc, !src) <- asyncPollSource frameQLength
                                     pollIntervall
                                     ptime
                                     (resampled16kHzS16RtpAlawUDPSource 10000
                                                                        "127.0.0.1"
                                                                        ptime
                                                                        frameQLength)
    !asink <- async (runConduit (src .| concealMissing blankFor .|
                                     streamDebugPlaybackSink))
    link2 asrc asink
    liftBase $ putStrLn "~~~ Press any key to exit ~~~"
    void $ liftBase getLine
    cancel asrc
    void $ waitCatch asink
    liftBase $ putStrLn "~~~ Exitting ~~~"
    return ()

resampled16kHzS16RtpAlawUDPSource :: MonadResource m
                                  => Int
                                  -> HostPreference
                                  -> NominalDiffTime
                                  -> Int
                                  -> Source m (Stream Rtp.RtpSsrc Rtp.RtpSeqNum (Ticks 16000 Word64) (SampleBuffer (S16 16000)))
resampled16kHzS16RtpAlawUDPSource !udpListenPort !udpListenIP !ptime !reorderBufferSize =
    udpDatagramSource useUtcClock udpListenPort udpListenIP .|
        rtpSource .|
        rtpPayloadDemux [ (8, alawPayloadHandler) ] mempty .|
        transcodeStreamC' .|
        resample8to16kHz' (MkS16 0 :: S16 8000) .|
        convertTicksC' at8kHzU32 at16kHzU64 .|
        reorderFramesBySeqNumC reorderBufferSize .|
        repacketizeC ptime

asyncPollSource :: (Default c, HasDuration c, NFData c, MonadBaseControl IO m, KnownNat r, Integral t, Integral s, Random i, Random t, Random s)
                => Int
                -> NominalDiffTime
                -> NominalDiffTime
                -> Source (ResourceT m) (Stream i s (Ticks r t) c)
                -> m ( Async (StM m ())
                     , Source m (Stream i s (Ticks r t) (Discontinous c))
                     )
asyncPollSource !frameQueueLen !pollIntervall !pTime !src = do
    !ringRef <- liftBase $
                    newTVarIO (Ring.newRingBuffer frameQueueLen `using` rdeepseq)
    !a <- async (runConduitRes (src .| pushPayloadSink ringRef))
    return (a, pollPayloadSource pollIntervall pTime ringRef)

--  TODO create a gap detection mechanism, a simple stateful conduit that knows the next timestamp
pushPayloadSink :: MonadBaseControl IO m
                => TVar (Ring.RingBuffer (Maybe a))
                -> Sink (Stream i s t a) m ()
pushPayloadSink !ringRef =
    awaitForever go
  where
    go !x = maybe (return ()) pushInRing (x ^? payload)
      where
        pushInRing !v = liftBase $
            atomically $
                modifyTVar ringRef $ withStrategy rdeepseq $ Ring.push $ Just v

pollPayloadSource :: (Default c, NFData c, Random i, HasDuration c, MonadBaseControl IO m, KnownNat r, Integral t, Integral s, Random t, Random s)
                  => NominalDiffTime
                  -> NominalDiffTime
                  -> TVar (Ring.RingBuffer (Maybe c))
                  -> Source m (Stream i s (Ticks r t) (Discontinous c))
pollPayloadSource !pollIntervall !pTime !ringRef =
    evalStateC (0, 0, 0) $ do
        setRandomStartCtx
        yieldStart
        forever (do
                     !restTime <- use _3
                     (!bufs, !dt') <- pollNextBuffers restTime
                     _3 .= dt'
                     mapM_ yieldNextBuffer bufs)
  where
    setRandomStartCtx = do
        !ts0 <- liftBase randomIO
        !sn0 <- liftBase randomIO
        _1 .= ts0
        _2 .= sn0

    yieldStart = MkFrameCtx <$> liftBase randomIO <*> use _1 <*> use _2 >>=
        yield . MkStream . Start


    pollIntervallMicros :: Ticks 1000000 Int
    pollIntervallMicros = nominalDiffTime # pollIntervall

    pollNextBuffers !restTime =
        liftBase $ do
            !(t0 :: Time UtcClock) <- now
            let !pollIntervallAdjustment =
                    nominalDiffTime # restTime
            threadDelay (_ticks (pollIntervallMicros - pollIntervallAdjustment))
            !dt <- _utcTimeDiff <$> timeSince t0
            (!bufs, !dt', !timeMissing) <- atomically $ do
                                                     !ring <- readTVar ringRef
                                                     let (!bufs, !ring', !dt', !timeMissing) =
                                                             popUntil ring dt
                                                             `using`
                                                             rdeepseq
                                                     writeTVar ringRef ring'
                                                     return ( bufs
                                                            , dt'
                                                            , timeMissing
                                                            )
            when (timeMissing > 0)
                 (traceM (printf "*** RING UNDERFLOW wanted: %s missing: %s ***"
                                 (show dt)
                                 (show timeMissing)))
            return (bufs, dt')
      where
        popUntil !ring0 !dt0 = popUntil_ ring0 dt0 [] 0
          where
            popUntil_ !ring !dt !acc !timeMissing
                | dt < pTime = (reverse acc, ring, dt, timeMissing)
                | Ring.size ring > 0 = let (!mbuf, !ring') = Ring.popAndSet Nothing
                                                                            ring
                                       in
                                           case mbuf of
                                               Nothing -> popUntil_ ring'
                                                                    (dt - pTime)
                                                                    (Missing pTime :
                                                                         acc)
                                                                    (timeMissing +
                                                                         pTime)
                                               Just !buf -> popUntil_ ring'
                                                                      (dt -
                                                                           getDuration buf)
                                                                      (Got buf :
                                                                           acc)
                                                                      timeMissing
                | otherwise = popUntil_ ring
                                        (dt - pTime)
                                        (Missing pTime : acc)
                                        (timeMissing + pTime)



    yieldNextBuffer !buf = do
        !ts <- _1 <<+= nominalDiffTime # getDuration buf
        !sn <- _2 <<+= 1
        yield $
            MkStream $
                Next $
                    MkFrame ts sn buf

_receiveRtpFromUDPStreamType :: Proxy (Stream Rtp.RtpSsrc Rtp.RtpSeqNum DemoTicks (SampleBuffer (S16 DemoRate)))
_receiveRtpFromUDPStreamType =
    Proxy

type DemoTicks = Ticks DemoRate Word64

type DemoRate = 16000
