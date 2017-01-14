module Data.MediaBus.RtpSpec ( spec ) where

import           Data.Conduit
import           Data.Word
import           Test.Hspec
import           Test.QuickCheck
import           Data.Function
import           Data.Time.Clock
import           Data.Default
-- import Data.Conduit.Lift
import           Control.Monad.State.Strict
import           Data.Ord                   ( Ordering, comparing )

spec :: Spec
spec = do
    describe "incoming packet handling" $ do
        describe "packets with unknown SSRC" $ do
            it "just pushes packets to the end of the ring" $
                property $
                    True
            it "rembers the unknown SSRC as the next known SSRC" $
                property $
                    True
        describe "packets with known SSRC" $ do
            it "drops packets that are too old" $
                property $
                    True
            it "stores packets into the ring at the position given through the sequence number" $
                property $ True
    describe "pulling packets" $ do
        it "returns all packets" $ property $ True
        it "fills the gaps with packet loss indications" $ property $ True

-- =================================
-- Network Driven Rtp:
-- per received network packet:
--   Assign NTP timestamp
--   stupid simple SSRC handling
--      --> manage a list of MediaConduits one for each SSRC
--      --> maybe enforce an upper limit on the number of concurrent SSRCs (maybe 2)
--      --> ssrcDemux :: Monad m => (forall s. Default s => Ssrc -> Conduit MediaData (StateT s m) MediaData)
--                               -> Conduit MediaData m MediaData
-- -----------------------------------------------------
type RtpMediaStreamEvent = MediaStreamEvent RtpSsrc RtpSequence RtpClock

-- mkRtpMediaStreamEvent :: RtpSequenceNumber -> RtpTimestamp -> RtpPayloadType -> RtpMediaStreamEvent
-- mkRtpMediaStreamEvent = MkMediaStreamEvent

type RtpMediaStream = MediaStream RtpSsrc RtpSequence RtpClock

mkRtpMediaStream :: RtpSsrc -> RtpSequence -> RtpClock -> RtpMediaStream
mkRtpMediaStream = MkMediaStream

newtype RtpSsrc = MkRtpSsrc Word32
    deriving (Show, Eq)

type RtpSequence = Sequence RtpSequenceNumber

newtype RtpSequenceNumber = MkRtpSequenceNumber Word16
    deriving (Show, Bounded, Integral, Num, Enum, Real, Ord, Eq)

mkRtpSequence :: RtpSequenceNumber -> RtpSequence
mkRtpSequence = MkSequence

type RtpClock = Clock RtpTimestamp

newtype RtpTimestamp = MkRtpTimestamp Word32
    deriving (Show, Bounded, Integral, Num, Enum, Real, Ord, Eq)

mkRtpClock8kHz :: RtpTimestamp -> RtpClock
mkRtpClock8kHz = mkClock 8000


data RtpPayloadType = G711U | G711A deriving (Show, Eq)

data RawRtpPacket = MkRawRtpPacket { rawRtpSequenceNumber :: RtpSequenceNumber
                                   , rawRtpTimestamp :: RtpTimestamp
                                   , rawRtpSsrc :: RtpSsrc
                                   , rawRtpPayloadType :: RtpPayloadType
                                   , rawRtpPayload :: String
                                   } deriving Show

-- rawRtpToMediaStream :: RawRtpPacket -> (RtpMediaStream, RtpMediaStreamEvent)

-- -----------------------------------------------------

-- type MultiplexedMediaStreamEvent src seq clk =
-- -----------------------------------------------------
data MediaStreamEvent sequence clock payload =
      MkMediaStreamEvent { sequenceNumber :: SequenceElement sequence
                         , timestamp      :: Timestamp clock
                         , payload        :: payload
                         }

deriving instance
         (Show (SequenceElement s), Show (SequenceElement c), Show p) =>
         Show (MediaStreamEvent s c p)

data MediaStream sourceId sequence clock =
      MkMediaStream { sourceId :: sourceId
                    , sequence :: sequence
                    , clock    :: clock
                    }
    deriving (Show)

-- -------------------------------------
class IsSequence s where
    type SequenceElement s
    seqAdd :: Integral b => s -> b -> SequenceElement s -> SequenceElement s
    seqPos :: Integral b => s -> SequenceElement s -> b

seqCmp :: forall s.
       IsSequence s
       => s
       -> SequenceElement s
       -> SequenceElement s
       -> Ordering
seqCmp = comparing . (seqPos :: s -> SequenceElement s -> Integer)

seqDiff :: (Integral b, IsSequence s)
        => s
        -> SequenceElement s
        -> SequenceElement s
        -> b
seqDiff s = (-) `on` seqPos s

newtype Sequence w = MkSequence w
    deriving (Show)

instance (Bounded w, Integral w) =>
         IsSequence (Sequence w) where
    type SequenceElement (Sequence w) = w
    seqAdd _ x y = fromIntegral x + y
    seqPos (MkSequence start) x =
        fromIntegral (if x < start then maxBound - (start - x) else x - start)

-- --------------------------------------
class IsSequence c =>
      IsClock c where
    tickDuration :: c -> DiffTime

type Timestamp c = SequenceElement c

timeOf :: IsClock c => c -> SequenceElement c -> DiffTime
timeOf c t = fromInteger (seqPos c t) * tickDuration c

clockDiffTime :: IsClock c
              => c
              -> SequenceElement c
              -> SequenceElement c
              -> DiffTime
clockDiffTime c t1 t0 = fromInteger (seqDiff c t1 t0) * tickDuration c

data Clock t = MkClock { clockSequence     :: Sequence t
                       , clockTickDuration :: DiffTime
                       }
    deriving (Show)

instance (Bounded t, Integral t) =>
         IsSequence (Clock t) where
    type SequenceElement (Clock t) = t
    seqAdd = seqAdd . clockSequence
    seqPos = seqPos . clockSequence

instance (Bounded t, Integral t) =>
         IsClock (Clock t) where
    tickDuration = clockTickDuration

mkClock :: Integral a => a -> a -> Clock a
mkClock ticksPerSecond startValue =
    MkClock (MkSequence startValue) (1 / fromIntegral ticksPerSecond)-- ---------------------------------- WIP Below
                                                                     -- -- | Stateful stream handling
                                                                     -- multiStream :: (Default s, Monad m)
                                                                     --             => (MediaStreamId -> Conduit MediaData (StateT s m) MediaData)
                                                                     --             -> Conduit MediaStream m MediaStream
                                                                     -- multiStream init = undefined
                                                                     -- --      --> Then assume: type MediaFunction = forall m. Monad m => Conduit MediaData m MediaData
                                                                     -- --          and also a simple MediaMixer like:
                                                                     --
                                                                     --   Packet Loss Detection, Packet Ordering and Jitter
                                                                     --      -->
                                                                     --   create/use decoder for specific payload type
                                                                     --      --> (forall  s t. (Default s, Monad m) => (RtpPayloadType -> Conduit MediaData (StateT s m) MediaData) -> Conduit MediaData m MediaData)
                                                                     --     aggregate media data in a Set
                                                                     --        --> Sort by
                                                                     -- =================================
                                                                     -- =================================
                                                                     -- Periodic Conduit over a Monadic action (the above for instance)
                                                                     --   sleep for some time
                                                                     --   initialize the start timestamp map
                                                                     --   copy all elements from the ring into the aggregation buffer
                                                                     --      if an element is "old" or missing
                                                                     --           then remember the current start position in the gap list
                                                                     --           else convert the start
                                                                     -- ---------------------------------
                                                                     -- =================================
                                                                     -- Re-framer:
                                                                     --  reframer :: Source m o -> (o -> m (Reframed o'))
                                                                     -- A downstream event listener
                                                                     --  encode
                                                                     --  aggregate
                                                                     --  frame/multiplex
