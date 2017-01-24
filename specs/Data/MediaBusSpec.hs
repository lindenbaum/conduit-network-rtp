module Data.MediaBusSpec ( spec ) where

import           Conduit                    as C
import           Data.Conduit.List          ( consume, sourceList )
import           Data.Function
import           Data.List                  ( sort )
import           Data.Word
import           Test.Hspec
import           Test.QuickCheck
import           Data.Time.Clock
import           Data.MediaBus

-- ----------------------------------------------------------------------
-- * Rtp Prototype
-- ----------------------------------------------------------------------
newtype RtpSsrc = MkRtpSsrc Word32
    deriving (Show, Eq)

newtype RtpSequence = MkRtpSequence Word16
    deriving (Show, Bounded, Integral, Num, Enum, Real, Ord, Eq)

newtype RtpTimestamp = MkRtpTimestamp Word32
    deriving (Show, Bounded, Integral, Num, Enum, Real, Ord, Eq)

data RtpPayload = MkRtpPayload { rtpPayload     :: String
                               , rtpPayloadType :: RtpPayloadType
                               }
    deriving Show

newtype RtpPayloadType = MkRtpPayloadType { fromRtpPayloadType :: Word8 }
    deriving (Show, Eq, Num)

data RawRtpPacket = MkRawRtpPacket { rawRtpSequenceNumber :: RtpSequence
                                   , rawRtpTimestamp      :: RtpTimestamp
                                   , rawRtpSsrc           :: RtpSsrc
                                   , rawRtpPayload        :: RtpPayload
                                   }
    deriving Show

type RawRtpPacketSource = NetworkSource RawRtpPacket

data RtpClockReference =
      RtpTimestampReference Clock (Offset RtpTimestamp)
    | RtpSequenceReference (Offset RtpSequence)
    deriving Show

type RtpSample = Sample (RtpSequence, RtpTimestamp) RtpPayload

newtype OnRtpSequence = MkOnRtpSequence { unOnRtpSequence :: RtpSample }

type SyncRtpSample = SynchronizedTo RtpClockReference RtpSample

type SyncRtpSource = IdentifiedBy RtpSsrc SyncRtpSample

data IpPort = MkIpPort

data NtpTime = MkNtpTime

data RawData = MkRawData

type NetworkSource a = IdentifiedBy IpPort (UTCSyncSample a)

type UTCSyncSample a = SynchronizedTo (Offset UTCTime) (Sample DiffTime a)

type RawDataNetworkSource = NetworkSource RawData

-- -----------------------------------------------------------------------------
-- * Tests/Specs
-- -----------------------------------------------------------------------------
spec :: Spec
spec = do
    reorderSpec
    addSequenceNumberSpec

reorderSpec :: Spec
reorderSpec = describe "reorder" $ do
    it "the output is always monotonic increasing" $
        property reorderOutputIsMonotoneIncreasing
    it "if the input is non-empty the output is too" $
        property reorderOutputOnlyEmptyIfInputEmpty
    it "works even if the index wraps around" $
        property $
            \(Positive windowSize) ->
                let outSamples = runConduitPure (sourceList inSamples .|
                                                     reorder windowSize id .|
                                                     consume)
                    inSamples = [ 253, 254, 255, 0, 1 :: Word8 ]
                in
                    outSamples `shouldBe` inSamples

reorderOutputIsMonotoneIncreasing :: [Monotone Word8]
                                  -> Positive Int
                                  -> Expectation
reorderOutputIsMonotoneIncreasing inSamples (Positive windowSize) =
    let outSamples = runConduitPure (sourceList inSamples .|
                                         reorder windowSize id .|
                                         consume)
    in
        outSamples `shouldBe` sort outSamples

reorderOutputOnlyEmptyIfInputEmpty :: [Monotone Word8]
                                   -> Positive Int
                                   -> Expectation
reorderOutputOnlyEmptyIfInputEmpty inSamples (Positive windowSize) =
    let outSamples = runConduitPure (sourceList inSamples .|
                                         reorder windowSize id .|
                                         consume)
    in
        null inSamples `shouldBe` null outSamples

addSequenceNumberSpec :: Spec
addSequenceNumberSpec = describe "addSequenceNumber" $
    it "produces dense, strictly monotonic output" $
        property addSequenceNumberIsMonotone

addSequenceNumberIsMonotone :: (NonEmptyList [Bool]) -> Word64 -> Expectation
addSequenceNumberIsMonotone (NonEmpty xs) startVal =
    let inEvents = sourceList xs
        (first : rest) = runConduitPure (inEvents .|
                                             addSequenceNumber (MkOffset startVal) .|
                                             consume)
    in do
        first `shouldBe`
            SynchronizeTo (MkOffset startVal) (MkSample startVal (head xs))
        (rest `zip` drop 1 rest) `shouldSatisfy`
            all (not .
                     uncurry (succeeds `on` _presentationTime . fromSynchronized))
