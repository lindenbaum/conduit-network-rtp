module Data.MediaBus.RtpSpec ( spec ) where

import           Data.Conduit
import           Data.Conduit.List          ( consume, sourceList )
import           Data.Conduit.Lift
import           Data.Function
import           Data.List                  ( sort )
import qualified Data.Set                   as Set
import           Data.Word
import           Test.Hspec
import           Test.QuickCheck
import           Data.Time.Clock
-- -- import           Data.Default
import           Control.Monad.State.Strict
import           Control.Lens
import           GHC.TypeLits

spec :: Spec
spec = do
    reorderSpec
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

reorderSpec :: Spec
reorderSpec = describe "reorder" $ do
    it "the output is always monotonic increasing" $
        property reorderOutputIsMonotoneIncreasing
    it "if the input is non-empty the output is too" $
        property reorderOutputOnlyEmptyIfInputEmpty

reorderOutputIsMonotoneIncreasing :: [Word8] -> Positive Int -> Expectation
reorderOutputIsMonotoneIncreasing inSamples (Positive windowSize) =
    let outSamples = runConduitPure (sourceList inSamples .|
                                         reorder windowSize id .|
                                         consume)
    in
        outSamples `shouldBe` sort outSamples

reorderOutputOnlyEmptyIfInputEmpty :: [Word8] -> Positive Int -> Expectation
reorderOutputOnlyEmptyIfInputEmpty inSamples (Positive windowSize) =
    let outSamples = runConduitPure (sourceList inSamples .|
                                         reorder windowSize id .|
                                         consume)
    in
        null inSamples `shouldBe` null outSamples

resynchronizeToSpec :: Spec
resynchronizeToSpec = describe "resynchronizeTo" $
    it "resynchronizeTo produces dense, strictly monotonic output, for dense, strictly monotonic input" $
        property resynchronizeToIsMonotone

resynchronizeToIsMonotone :: [(Word8, Word8)] -> Expectation
resynchronizeToIsMonotone ranges =



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
      RtpTimestampReference Clock (Offset RtpTimestamp)
    | RtpSequenceReference (Offset RtpSequence)
    deriving Show

type RtpSample = Sample (RtpSequence, RtpTimestamp) RtpPayload

newtype OnRtpSequence = MkOnRtpSequence { unOnRtpSequence :: RtpSample }

type SyncRtpSample = SynchronizedTo RtpClockReference RtpSample

type SyncRtpSource = IdentifiedBy RtpSsrc SyncRtpSample

-- ----------------------------------------------------------------------
-- * Raw network sources
-- ----------------------------------------------------------------------
data IpPort = MkIpPort

data NtpTime = MkNtpTime

data RawData = MkRawData

type NetworkSource a = IdentifiedBy IpPort (UTCSyncSample a)

type UTCSyncSample a = SynchronizedTo (Offset UTCTime) (Sample DiffTime a)

type RawDataNetworkSource = NetworkSource RawData

-- -----------------------------------------------------
-- * Media Data Synchronization
-- -----------------------------------------------------
-- | Buffer incoming samples in a queue of the given size and output them
-- ordered by 'presentationTime'. The output is guaranteed to be monotone
-- increasing.
reorder :: (Integral t, Monad m) => Int -> (a -> t)  -> Conduit a m a
reorder windowSize indexOf =
    go Set.empty Nothing
  where
    go queue minIndex = do
        mx <- await
        case mx of
            Nothing -> mapM_ (yield . _sampleData) queue
            Just x -> if maybe False (indexOf x <=) minIndex
                      then go queue minIndex
                      else let queue' = Set.insert (MkSample (indexOf x) x)
                                                   queue
                               mMinView = Set.minView queue'
                           in
                               if Set.size queue >= windowSize
                               then mapM_ (yield . _sampleData . fst) mMinView
                                   >> go (maybe queue' snd mMinView)
                                         (maybe minIndex
                                                (Just .
                                                     _presentationTime . fst)
                                                mMinView)
                               else go queue' minIndex

-- | Synchronized version of `reorder`. It
-- | Adapt the clock/timetstamp to another /outter/ static clock. Incoming
--  'SynchronizeTo' packets are processed, the output is always synchronous
--  to the given clock.
resynchronizeTo :: forall m tIn tOut pIn pOut.
                (Monad m, Integral tIn, Integral tOut)
                => Offset tOut
                -> (forall pIn' tIn'. Lens pIn pIn' tIn tIn')
                -> Conduit (SynchronizedTo (Offset tIn) pIn) m (SynchronizedTo (Offset tOut) pOut)
resynchronizeTo tOutRef timestampIn =
    await >>= maybe (return ()) (begin >=> (`evalStateC` awaitForever go))
  where
    begin p@(Synchronized pIn) = do
        let tInRef = MkOffset (fromIntegral (view timestampIn pIn))
        leftover p
        return tInRef
    begin (SynchronizeTo tInRef) =
        return tInRef

    go (Synchronized pIn) = do
        MkOffset tInRef <- get
        let toTOut tIn = fromIntegral (tIn - tInRef) + fromOffset tOutRef
        yield (Synchronized (over timestampIn toTOut pIn))
    go (SynchronizeTo tInRef) =
        put tInRef

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
data Sample t s = MkSample { _presentationTime :: t
                           , _sampleData       :: s
                           }
    deriving (Show)

instance Eq i =>
         Eq (Sample i c) where
    (==) = (==) `on` _presentationTime

instance Ord i =>
         Ord (Sample i c) where
    compare = compare `on` _presentationTime

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
newtype Offset s = MkOffset { fromOffset :: s }
    deriving (Show, Bounded, Integral, Num, Enum, Real, Ord, Eq, Arbitrary)

-- --------------------------------------
-- --------------------------------------
class IsClock c where
    tickDuration :: c -> NominalDiffTime

-- | Return the number of timestamp that a temopral value represents
timeOf :: (IsClock c, Integral t) => c -> t -> NominalDiffTime
timeOf c t = fromInteger (toInteger t) * tickDuration c

toRate :: (IsClock c, Integral t) => c -> t
toRate c = round (recip (tickDuration c))

clockDiffTime :: (IsClock c, Integral t) => c -> t -> t -> NominalDiffTime
clockDiffTime c t1 t0 = timeOf c t1 - timeOf c t0

-- --------------------------------------
data StaticClock (rate :: Nat) = MkStaticClock

instance (KnownNat rate) =>
         IsClock (StaticClock rate) where
    tickDuration = fromInteger . natVal

-- --------------------------------------
newtype Clock = MkClock NominalDiffTime
    deriving (Show)

fromRate :: Integral t => t -> Clock
fromRate timestampPerSecond =
    MkClock (1 / fromIntegral timestampPerSecond)



-- -----------------------------------------------------------------------
-- Lenses
-- -----------------------------------------------------------------------
makeLenses ''Sample
