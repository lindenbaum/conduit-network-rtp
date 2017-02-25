module Data.MediaBus.Internal.AsyncConduit
    ( mkDecoupledSource
    , connectConcurrentlyPolledSourceToSink
    , PayloadQ()
    , mkPayloadQ
    , payloadQSink
    , payloadQSource
    ) where

import           Control.Monad.State
import           Data.Time.Clock
import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.STM
import           Control.Concurrent                ( threadDelay )
import           Control.Parallel.Strategies       ( NFData, rdeepseq
                                                   , using, withStrategy )
import           GHC.TypeLits
import           System.Random
import           Conduit
import           Data.MediaBus
import qualified Data.MediaBus.Internal.RingBuffer as Ring
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

mkDecoupledSource :: (Default c, HasDuration c, NFData c, MonadBaseControl IO m, KnownNat r, Integral t, Integral s, Random i, Random t, Random s, MonadResource m, NFData s, NFData t)
                  => Int
                  -> NominalDiffTime
                  -> NominalDiffTime
                  -> Source m (Stream i s (Ticks r t) c)
                  -> m ( Async ()
                       , Source m (Stream i s (Ticks r t) (Discontinous c))
                       )
mkDecoupledSource !frameQueueLen !pollIntervall !pTime !src = do
    !ringRef <- mkPayloadQ frameQueueLen
    !a <- async (runConduit (src .| payloadQSink ringRef))
    return (void a, payloadQSource pollIntervall pTime ringRef)

connectConcurrentlyPolledSourceToSink :: forall c m r t s i.
               (Default c, HasDuration c, NFData c, MonadBaseControl IO m, KnownNat r, Integral t, Integral s, Random i, Random t, Random s, NFData t, NFData s)
               => Int
               -> NominalDiffTime
               -> NominalDiffTime
               -> Source m (Stream i s (Ticks r t) c)
               -> Sink (Stream i s (Ticks r t) (Discontinous c)) m ()
               -> m ()
connectConcurrentlyPolledSourceToSink frameQueueLen pollIntervall pTime src sink = do
    ringRef <- mkPayloadQ frameQueueLen
    void $
        race (src $$ payloadQSink ringRef)
             (payloadQSource pollIntervall pTime ringRef $$
                  sink)

newtype PayloadQ a = MkPayloadQ (TVar (Ring.RingBuffer (Maybe a)))

mkPayloadQ :: (MonadBaseControl IO m) => Int -> m (PayloadQ a)
mkPayloadQ qlen = liftBase (MkPayloadQ <$> newTVarIO (Ring.newRingBuffer qlen))

payloadQSink :: (NFData a, MonadBaseControl IO m)
                => PayloadQ a
                -> Sink (Stream i s t a) m ()
payloadQSink (MkPayloadQ !ringRef) =
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
                  => NominalDiffTime
                  -> NominalDiffTime
                  -> PayloadQ c
                  -> Source m (Stream i s (Ticks r t) (Discontinous c))
payloadQSource pollIntervall pTime (MkPayloadQ ringRef) =
    evalStateC (MkPollPayloadSourceSt 0 0 0) $ do
        yieldStart
        go
  where
    go =
        do
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
            !(t0 :: Time UtcClock) <- now
            let !pollIntervallAdjustment =
                    nominalDiffTime # restTime
            threadDelay (_ticks (pollIntervallMicros - pollIntervallAdjustment))
            !dt <- _utcTimeDiff <$> timeSince t0
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
