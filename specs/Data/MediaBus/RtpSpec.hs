module Data.MediaBus.RtpSpec ( spec ) where

import           Data.Conduit
import           Data.Conduit.Lift
import           Data.Word
import           Data.Int
import           Test.Hspec
import           Test.QuickCheck
import           Data.Time.Clock
import           Data.Ix
import           Data.Function
-- -- import           Data.Default
import           Control.Monad.State.Strict
import qualified Data.Set                   as Set
import           GHC.TypeLits

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
    describe "relative sequences" $ do
        it "relativePosition ref . absolutePosition ref == id" $
            property $
                \(ref :: ReferencePosition Int8) v ->
                    relativePosition ref (absolutePosition ref v) `shouldBe`
                        v

-- ----------------------------------------------------------------------
-- * Rtp sources
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
      RtpTimestampReference Clock (ReferencePosition RtpTimestamp)
    | RtpSequenceReference (ReferencePosition RtpSequence)
    deriving Show

type RtpSample = Sample (RtpSequence, RtpTimestamp) RtpPayload

type SyncRtpSample = SynchronizedTo RtpClockReference RtpSample

type SyncRtpSource = IdentifiedBy RtpSsrc SyncRtpSample

-- ----------------------------------------------------------------------
-- * Raw network sources
-- ----------------------------------------------------------------------
data IpPort

data NtpTime

data RawData

type NetworkSource a = IdentifiedBy IpPort (UTCSyncSample a)

type UTCSyncSample a = SynchronizedTo (ReferencePosition UTCTime) (Sample DiffTime a)

type RawDataNetworkSource = NetworkSource RawData

-- -----------------------------------------------------
-- * Media Data Processing
-- -----------------------------------------------------
reorder :: (Ord t, Monad m) => Int -> Conduit (Sample t c) m (Sample t c)
reorder windowSize = go Set.empty Nothing
  where
    go :: (Ord t, Monad m)
       => Set.Set (Sample t c)
       -> Maybe t
       -> Conduit (Sample t c) m (Sample t c)
    go sampleQueue mMinTS = do
        msample <- await
        case msample of
            Nothing -> mapM_ yield sampleQueue
            Just sample ->
              if  maybe False (presentationTime sample <=) mMinTS then
                go sampleQueue mMinTS
              else
                let sampleQueue' = Set.insert sample sampleQueue
                    mMinView = Set.minView sampleQueue'
                in
                    if Set.size sampleQueue >= windowSize
                    then mapM_ (yield . fst) mMinView
                        >> go (maybe sampleQueue' snd mMinView)
                              (maybe mMinTS (Just . presentationTime . fst) mMinView)
                    else go sampleQueue' mMinTS

-- -----------------------------------------------------
-- * Media Data Processing Base Types
-- -----------------------------------------------------
-- | 'Event's for things that need to be initialized with a 'Clock' to
-- synchronize to.
data SynchronizedTo c t =
      SynchronizeTo c
    | Synchronized t
    deriving Show

instance Functor (SynchronizedTo c) where
    fmap _ (SynchronizeTo c) =
        SynchronizeTo c
    fmap f (Synchronized x) =
        Synchronized (f x)

type Clocked p = SynchronizedTo Clock p

-- | Things that can be uniquely identified by a looking at a (much simpler)
-- representation, the 'identity'.
data IdentifiedBy i c = MkIdentifiedBy { identifier   :: i
                                       , unIdentified :: c
                                       }
    deriving (Show)

instance Functor (IdentifiedBy i) where
    fmap f (MkIdentifiedBy i c) =
        MkIdentifiedBy i (f c)

-- | A 'Sample' can be anything that has a start time and is exactly one time
-- unit long, it can respresent anything ranging from an audio buffer with 20ms
-- of audio to a single pulse coded audio sample, of course it could also be a
-- video frame or a chat message.
data Sample t s = MkSample { presentationTime :: t
                           , sampleData       :: s
                           }
    deriving (Show)

instance Ord t =>
         Ord (Sample t s) where
    compare = compare `on` presentationTime

instance Eq t =>
         Eq (Sample t s) where
    (==) = (==) `on` presentationTime

instance Functor (Sample t) where
    fmap f (MkSample t x) = MkSample t (f x)

-- | Indication of a state change in a stream
data Event a b = Init a
               | Handle b
               | Terminate
    deriving (Show)

instance Functor (Event a) where
    fmap f (Handle x) = Handle (f x)
    fmap _f (Init x) = Init x
    fmap _f Terminate = Terminate

-- -----------------------------------------------------
-- * Dealing with gaps in media streams
-- -----------------------------------------------------
-- | Differentiate between continuous and non-continous occurences of something.
data Discontinous a b = Gap a
                      | Continue b
    deriving (Show)

instance Functor (Discontinous a) where
    fmap f (Continue x) = Continue (f x)
    fmap _f (Gap x) = Gap x

-- -------------------------------------
newtype ReferencePosition s = MkReferencePosition { referenceValue :: s }
    deriving (Show, Bounded, Integral, Num, Enum, Real, Ord, Eq, Arbitrary)

newtype RelativePosition s = MkRelativePosition { fromRelativePosition :: s }
    deriving (Show, Bounded, Integral, Num, Enum, Real, Ord, Eq, Arbitrary)

absolutePosition :: (Integral s, Bounded s)
                 => ReferencePosition s
                 -> RelativePosition s
                 -> s
absolutePosition MkReferencePosition{referenceValue} MkRelativePosition{fromRelativePosition} =
    fromIntegral $
        if fromRelativePosition < referenceValue
        then maxBound - (referenceValue - fromRelativePosition)
        else fromRelativePosition - referenceValue

relativePosition :: (Integral s)
                 => ReferencePosition s
                 -> s
                 -> RelativePosition s
relativePosition y xr = MkRelativePosition (fromIntegral y + fromIntegral xr)

-- --------------------------------------
-- --------------------------------------
class IsClock c where
    tickDuration :: c -> NominalDiffTime

toRate :: (IsClock c, Integral t) => c -> t
toRate c = round (recip (tickDuration c))

timeOf :: (IsClock c, Integral t) => c -> t -> NominalDiffTime
timeOf c t = fromIntegral t * tickDuration c

clockDiffTime :: (IsClock c, Integral t) => c -> t -> t -> NominalDiffTime
clockDiffTime c t1 t0 = let d = tickDuration c
                        in
                            fromInteger (toInteger (t1 - t0)) * d

-- --------------------------------------
data StaticClock (rate :: Nat) = MkStaticClock

instance (KnownNat rate) =>
         IsClock (StaticClock rate) where
    tickDuration = fromInteger . natVal

-- --------------------------------------
newtype Clock = MkClock NominalDiffTime
    deriving (Show)

fromRate :: Integral t => t -> Clock
fromRate ticksPerSecond =
    MkClock (1 / fromIntegral ticksPerSecond)
