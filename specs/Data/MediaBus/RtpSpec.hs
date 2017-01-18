module Data.MediaBus.RtpSpec ( spec ) where

-- import           Data.Conduit
import           Data.Word
import           Data.Int
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
    sequentialSpec
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

sequentialSpec :: Spec
sequentialSpec = do
    describe "Monotonic instance" $ do
        it "relativePosition ref . absolutePosition ref == id" $
            property $
                \(ref :: ReferencePosition (Monotonic Int8)) v ->
                    relativePosition ref (absolutePosition ref v) `shouldBe`
                        v

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

--
type RtpMediaStreamEvent = MediaStreamEvent RtpSsrc RtpSequence RtpClock

-- mkRtpMediaStreamEvent :: RtpSequenceNumber -> RtpTimestamp -> RtpPayloadType -> RtpMediaStreamEvent
-- mkRtpMediaStreamEvent = MkMediaStreamEvent
-- type RtpMediaEvent = MediaStream RtpSsrc RtpSequence RtpClock
-- mkRtpMediaStream :: RtpSsrc -> RtpSequence -> RtpClock -> RtpMediaStream
-- mkRtpMediaStream = MkMediaStream
newtype RtpSsrc = MkRtpSsrc Word32
    deriving (Show, Eq)

type RtpSequence = ReferencePosition RtpSequenceNumber

newtype RtpSequenceNumber = MkRtpSequenceNumber (Monotonic Word16)
    deriving (Show, Bounded, Integral, Num, Enum, Real, Ord, Eq, IsSequential)

mkRtpSequence :: RtpSequenceNumber -> RtpSequence
mkRtpSequence = MkReferencePosition

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
data StreamBufferQueue tStream tSample p =
      MkStreamBufferQueue { payloadQueue    :: Set.Set (Sample (AbsolutePosition tSample) p)
                          , firstSampleTime :: ReferencePosition tSample
                          , streamReference :: ReferencePosition tStream
                          }

newtype SessionId = MkSessionId (Word64, Word64)
    deriving (Show, Ord, Eq)

newtype SessionTimestamp = MkSessionTimestamp Word64
    deriving (Show, Ord, Eq, Num, Integral, Enum, Real, Bounded)

newtype SessionSequenceNumber = MkSessionSequenceNumber Word64
    deriving (Show, Ord, Eq, Num, Integral, Enum, Real, Bounded)

-- -----------------------------------------------------

-- -----------------------------------------------------
data MediaStreamEvent id s p =
      Reconfigure id (ReferencePosition s)
    | Process id (Sample s p)
    | Terminate id
    deriving (Show)



-- | A 'Sample' can be anything that has a start time and is exactly one time
-- unit long, it can respresent anything ranging from an audio buffer with 20ms
-- of audio to a single pulse coded audio sample, of course it could also be a
-- video frame or a chat message.
data Sample t s = MkSample { presentationTime :: t
                           , sampleData       :: s
                           }
    deriving (Show)

-- -------------------------------------
class IsSequential s where
    absolutePosition :: ReferencePosition s -> s -> AbsolutePosition s
    relativePosition :: ReferencePosition s -> AbsolutePosition s -> s
    comparePositions :: ReferencePosition s -> s -> s -> Ordering
    diffPositions :: ReferencePosition s -> s -> s -> AbsolutePosition s

-- --------------------------------------
instance (IsSequential s, IsSequential t) =>
         IsSequential (s, t) where
    absolutePosition (MkReferencePosition (sr, tr)) (s, t) =
        let MkAbsolute sPos = absolutePosition (MkReferencePosition sr) s
            MkAbsolute tPos = absolutePosition (MkReferencePosition tr) t
        in
            MkAbsolute (sPos, tPos)
    comparePositions (MkReferencePosition (sr, tr)) (xs, xt) (ys, yt) =
        let cmpS = comparePositions (MkReferencePosition sr) xs ys
            cmpT = comparePositions (MkReferencePosition tr) xt yt
        in
            compare (cmpS, EQ) (EQ, cmpT)
    diffPositions (MkReferencePosition (sr, tr)) (xs, xt) (ys, yt) =
        MkAbsolute ( fromAbsolutePosition $
                       diffPositions (MkReferencePosition sr) xs ys
                   , fromAbsolutePosition $
                       diffPositions (MkReferencePosition tr) xt yt
                   )
    relativePosition (MkReferencePosition (xsr, xtr)) (MkAbsolute (ys, yt)) =
        ( relativePosition (MkReferencePosition xsr) (MkAbsolute ys)
        , relativePosition (MkReferencePosition xtr) (MkAbsolute yt)
        )

-- --------------------------------------
newtype ReferencePosition s = MkReferencePosition { referenceValue :: s }
    deriving (Show, Bounded, Integral, Num, Enum, Real, Ord, Eq, Arbitrary)

newtype AbsolutePosition s = MkAbsolute { fromAbsolutePosition :: s }
    deriving (Show, Bounded, Integral, Num, Enum, Real, Ord, Eq, Arbitrary)

-- -------------------------------------
newtype Monotonic i = MkMonotonic i
    deriving (Eq, Show, Ord, Num, Real, Bounded, Enum, Integral, Arbitrary)

instance (Bounded i, Integral i) =>
         IsSequential (Monotonic i) where
    absolutePosition (MkReferencePosition (MkMonotonic referenceValue)) (MkMonotonic x) =
        fromIntegral $
            if x < referenceValue
            then maxBound - (referenceValue - x)
            else x - referenceValue
    comparePositions = comparing . absolutePosition
    diffPositions s = (-) `on` absolutePosition s
    relativePosition y xr = fromIntegral y + fromIntegral xr

-- --------------------------------------
data Clock t = MkClock { tickReference :: ReferencePosition t
                       , tickDuration  :: NominalDiffTime
                       }
    deriving (Show)

newtype ClockRate = MkClockRate Word64
    deriving (Show, Integral, Enum, Num, Real, Ord, Eq)

fromRate :: ClockRate -> t -> Clock t
fromRate ticksPerSecond ref =
    MkClock (MkReferencePosition ref) (1 / fromIntegral ticksPerSecond)

timeOf :: (Integral t, IsSequential t) => Clock t -> t -> NominalDiffTime
timeOf c t = fromIntegral (absolutePosition (tickReference c) t) *
    tickDuration c

clockDiffTime :: (Integral t, IsSequential t)
              => Clock t
              -> t
              -> t
              -> NominalDiffTime
clockDiffTime c t1 t0 = fromIntegral (diffPositions (tickReference c) t1 t0) *
    tickDuration c

-- --------------------------------------

data Continous t a = Continous { payload :: a }
                   | Interrupted { gapStart    :: t
                                 , gapDuration :: t
                                 , payload     :: a
                                 }
                   deriving (Show)

-- gapDetector :: (MonadState t m, Eq t, Temporal t a) => Clock t -> a -> m (Continous t a)
