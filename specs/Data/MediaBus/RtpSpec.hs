module Data.MediaBus.RtpSpec ( spec ) where

-- -- import           Data.Conduit
import           Data.Word
import           Test.Hspec
import           Test.QuickCheck
import           Data.Function
import           Data.Time.Clock
-- -- import           Data.Default
-- import Data.Conduit.Lift
-- -- import           Control.Monad.State.Strict
import           Data.Ord        ( Ordering, comparing )
import qualified Data.Set        as Set


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
-- type RtpMediaEvent = MediaStream RtpSsrc RtpSequence RtpClock
-- mkRtpMediaStream :: RtpSsrc -> RtpSequence -> RtpClock -> RtpMediaStream
-- mkRtpMediaStream = MkMediaStream
newtype RtpSsrc = MkRtpSsrc Word32
    deriving (Show, Eq)

type RtpSequence = SequenceReference RtpSequenceNumber

newtype RtpSequenceNumber = MkRtpSequenceNumber (Monotonic Word16)
    deriving (Show, Bounded, Integral, Num, Enum, Real, Ord, Eq, IsSequential)

mkRtpSequence :: RtpSequenceNumber -> RtpSequence
mkRtpSequence = MkSequenceReference

type RtpClock = Clock RtpTimestamp

newtype RtpTimestamp = MkRtpTimestamp (Monotonic Word32)
    deriving (Show, Bounded, Integral, Num, Enum, Real, Ord, Eq, IsSequential)

mkRtpClock8kHz :: RtpTimestamp -> RtpClock
mkRtpClock8kHz = fromRate 8000

data RtpPayloadType = G711U | G711A
    deriving (Show, Eq)

data RawRtpPacket = MkRawRtpPacket { rawRtpSequenceNumber :: RtpSequenceNumber
                                   , rawRtpTimestamp      :: RtpTimestamp
                                   , rawRtpSsrc           :: RtpSsrc
                                   , rawRtpPayloadType    :: RtpPayloadType
                                   , rawRtpPayload        :: String
                                   }
    deriving Show

-- rawRtpToEvent :: RawRtpPacket -> State RtpMediaStreamEvent
-- -----------------------------------------------------
--
-- Order of RTP packets:
-- Same SSRC: order by Rtp timestamp
-- Different SSRC order by Ntp timestamp
--
-- stream buffer queue
--   components:
--    * the PacketReference
--    * a bounded payload queue orderd by stream time stamp
--    * the global start time stamp of the stream
--
--   operations:
--    * gather media data packets
--    * generate missing elements
--
-- media packet:
--  components:
--    * sequence number
--    * timestamp
--    * media data payload
--
--  operations:
--    * filtering, transcoding, tone generation, dtmf/tone detection, voice
--      activity detection, noise reduction, resampling, mix/combine with other
--      stream media buffers
--
data StreamBufferQueue s p =
      MkStreamBufferQueue { payloadQueue    :: Set.Set (Packet s p)
                          , packetReference :: SequenceReference s
                          , streamReference :: SequenceReference s
                          }

newtype SessionId = MkSessionId (Word64, Word64)
    deriving (Show, Ord, Eq)

newtype SessionTimestamp = MkSessionTimestamp Word64
    deriving (Show, Ord, Eq, Num, Integral, Enum, Real, Bounded)

newtype SessionSequenceNumber = MkSessionSequenceNumber Word64
    deriving (Show, Ord, Eq, Num, Integral, Enum, Real, Bounded)

-- -----------------------------------------------------
data MediaStreamEvent id s p =
      Reconfigure id (SequenceReference s)
    | Process id (Packet s p)
    | Terminate id
    deriving (Show)

data Packet s p = MkPacket { sequenceNumber :: s
                           , payload        :: p
                           }
    deriving (Show)

-- -------------------------------------
class IsSequential s where
    getPosition :: SequenceReference s -> s -> s
    comparePositions :: SequenceReference s -> s -> s -> Ordering
    default comparePositions :: Ord s => SequenceReference s -> s -> s -> Ordering
    comparePositions = comparing . getPosition
    diffPositions :: SequenceReference s -> s -> s -> s
    default diffPositions :: Num s => SequenceReference s -> s -> s -> s
    diffPositions s = (-) `on` getPosition s
    rebasePosition :: SequenceReference s -> s -> SequenceReference s -> s
    default rebasePosition :: Integral s
     => SequenceReference s -> s -> SequenceReference s -> s
    rebasePosition yr y xr =
        getPosition yr y + fromIntegral xr

-- --------------------------------------

instance (IsSequential s, IsSequential t) =>
         IsSequential (s, t) where
    getPosition (MkSequenceReference (sr, tr)) (s, t) =
        let sPos = getPosition (MkSequenceReference sr) s
            tPos = getPosition (MkSequenceReference tr) t
        in
            (sPos, tPos)
    comparePositions (MkSequenceReference (sr, tr)) (xs, xt) (ys, yt) =
        let cmpS = comparePositions (MkSequenceReference sr) xs ys
            cmpT = comparePositions (MkSequenceReference tr) xt yt
        in
            compare (cmpS, EQ) (EQ, cmpT)
    diffPositions (MkSequenceReference (sr, tr)) (xs, xt) (ys, yt) =
        ( diffPositions (MkSequenceReference sr) xs ys
        , diffPositions (MkSequenceReference tr) xt yt
        )
    rebasePosition (MkSequenceReference (ysr, ytr)) (ys, yt) (MkSequenceReference (xsr, xtr)) =
        ( rebasePosition (MkSequenceReference ysr) ys (MkSequenceReference xsr)
        , rebasePosition (MkSequenceReference ytr) yt (MkSequenceReference xtr)
        )

-- --------------------------------------

newtype SequenceReference s = MkSequenceReference { referenceValue :: s }
    deriving (Show, Bounded, Integral, Num, Enum, Real, Ord, Eq)

data AbsolutePosition s = MkAbsolute (SequenceReference s) s
    deriving (Show)

instance (Eq s, IsSequential s) =>
         Eq (AbsolutePosition s) where
    (MkAbsolute xr x) == (MkAbsolute yr y) =
        x == rebasePosition yr y xr

instance (Eq s, IsSequential s) =>
         Ord (AbsolutePosition s) where
    compare (MkAbsolute xr x) (MkAbsolute yr y) =
        comparePositions xr x (rebasePosition yr y xr)

-- -------------------------------------

newtype Monotonic i = MkMonotonic i
    deriving (Eq, Show, Ord, Num, Real, Bounded, Enum, Integral)

instance (Bounded i, Integral i) =>
         IsSequential (Monotonic i) where
    getPosition (MkSequenceReference (MkMonotonic referenceValue)) (MkMonotonic x) =
        fromIntegral $
            if x < referenceValue
            then maxBound - (referenceValue - x)
            else x - referenceValue

-- --------------------------------------
data Clock t = MkClock { tickReference :: SequenceReference t
                       , tickDuration  :: DiffTime
                       }
    deriving (Show)

newtype ClockRate = MkClockRate Word64
    deriving (Show, Integral, Enum, Num, Real, Ord, Eq)

fromRate :: ClockRate -> t -> Clock t
fromRate ticksPerSecond ref =
    MkClock (MkSequenceReference ref) (1 / fromIntegral ticksPerSecond)

timeOf :: (Integral t, IsSequential t) => Clock t -> t -> DiffTime
timeOf c t = fromIntegral (getPosition (tickReference c) t) * tickDuration c

clockDiffTime :: (Integral t, IsSequential t) => Clock t -> t -> t -> DiffTime
clockDiffTime c t1 t0 = fromIntegral (diffPositions (tickReference c) t1 t0) *
    tickDuration c
