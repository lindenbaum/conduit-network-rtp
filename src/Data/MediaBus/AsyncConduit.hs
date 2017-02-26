module Data.MediaBus.AsyncConduit
    ( withAsyncPolledSource
    , PayloadQ()
    , mkPayloadQ
    , payloadQSink
    , payloadQSource
    ) where

import           Control.Monad.State
import           Data.Time.Clock
import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.STM
import           Control.Concurrent              ( threadDelay )
import           Control.Parallel.Strategies     ( NFData, rdeepseq, using
                                                 , withStrategy )
import           GHC.TypeLits
import           System.Random
import           Conduit
import           Data.MediaBus.Clock
import           Data.MediaBus.Stream
import           Data.MediaBus.Discontinous
import qualified Data.MediaBus.RingBuffer        as Ring
import           Control.Lens
import           Text.Printf
import           Data.Default
import           Debug.Trace

data PollPayloadSourceSt s t =
      MkPollPayloadSourceSt { _ppSeqNum     :: !s
                            , _ppTicks      :: !t
                            , _ppPollAdjust :: !NominalDiffTime
                            }

makeLenses ''PollPayloadSourceSt

withAsyncPolledSource :: (MonadResource m, MonadBaseControl IO m, KnownNat r, Integral t, Integral s, Default c, HasDuration c, NFData c, NFData s, NFData t, Random i, Random t, Random s)
                      => Int
                      -> NominalDiffTime
                      -> Source m (Stream i s (Ticks r t) c)
                      -> (( Async ()
                          , Source m (Stream i s (Ticks r t) (Discontinous c))
                          )
                          -> m o)
                      -> m o
withAsyncPolledSource !frameQueueLen !pTime !src !f = do
    !pq <- mkPayloadQ frameQueueLen pTime
    withAsync (runConduit (src .| payloadQSink pq))
              (\a -> f (void a, payloadQSource pq))

data PayloadQ a = MkPayloadQ { _payloadQPayloadMinDuration :: !NominalDiffTime
                             , _payloadQPollIntervall      :: !NominalDiffTime
                             , _payloadQRing               :: !(TVar (Ring.RingBuffer (Maybe a)))
                             }

mkPayloadQ :: MonadBaseControl IO m => Int -> NominalDiffTime -> m (PayloadQ a)
mkPayloadQ qlen payloadMinDuration =
    MkPayloadQ payloadMinDuration (fromIntegral qlen * 0.5 * payloadMinDuration) <$> liftBase (newTVarIO (Ring.newRingBuffer qlen))

payloadQSink :: (NFData a, MonadBaseControl IO m)
             => PayloadQ a
             -> Sink (Stream i s t a) m ()
payloadQSink (MkPayloadQ _ _ !ringRef) =
    awaitForever go
  where
    go !x = do
        maybe (return ()) pushInRing (x ^? payload)
        !_ring <- liftBase $ atomically $ readTVar ringRef
        return ()
      where
        pushInRing = liftBase .
            atomically .
                modifyTVar ringRef . Ring.push . Just

payloadQSource :: (Random i, NFData c, HasDuration c, MonadBaseControl IO m, KnownNat r, Integral t, Integral s, NFData t, NFData s)
               => PayloadQ c
               -> Source m (Stream i s (Ticks r t) (Discontinous c))
payloadQSource (MkPayloadQ pTime pollIntervall ringRef) =
    evalStateC (MkPollPayloadSourceSt 0 0 0) $ do
        yieldStart
        go
  where
    go = do
        !restTime <- use ppPollAdjust
        (!bufs, !dt') <- pollNextBuffers restTime
        ppPollAdjust .= dt'
        mapM_ yieldNextBuffer bufs
        go
    yieldStart = (MkFrameCtx <$> liftBase randomIO
                             <*> use ppTicks
                             <*> use ppSeqNum) >>=
        yieldStartFrameCtx


    pollIntervallMicros :: Ticks 1000000 Int
    pollIntervallMicros = nominalDiffTime # pollIntervall

    pollNextBuffers !restTime =
        liftBase $ do
            !(t0 :: ClockTime UtcClock) <- now
            let !pollIntervallAdjustment =
                    nominalDiffTime # restTime
            threadDelay (_ticks (pollIntervallMicros - pollIntervallAdjustment))
            !dt <- _utcClockTimeDiff <$> timeSince t0
            (!bufs, !dt', !timeMissing, !ringSize) <- atomically $ do
                                                          !ring <- readTVar ringRef
                                                          let (!bufs, !ring', !dt', !timeMissing) =
                                                                  popUntil ring
                                                                           dt
                                                                  `using`
                                                                  rdeepseq
                                                          writeTVar ringRef
                                                                    ring'
                                                          return ( bufs
                                                                 , dt'
                                                                 , timeMissing
                                                                 , Ring.size ring
                                                                 )
            when (timeMissing > 0 && ringSize > 0)
                 (traceM (printf "*** RING UNDERFLOW wanted: %s missing: %s ***"
                                 (show dt)
                                 (show timeMissing)))
            return (bufs, dt')
      where
        popUntil !ring0 !dt0 = withStrategy rdeepseq (popUntil_ ring0 dt0 [] 0)
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
        let !bufferDuration = withStrategy rdeepseq $
                nominalDiffTime # getDuration buf
        !ts <- ppTicks <<+= bufferDuration
        !sn <- ppSeqNum <<+= 1
        yieldNextFrame $
            withStrategy rdeepseq $
                MkFrame ts sn buf
