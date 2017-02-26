module Data.MediaBus.Clock
    ( HasDuration(..)
    , HasTimestampT(..)
    , HasTimestamp(..)
    , Ticks(..)
    , mkTicks
    , at8kHzU32
    , at16kHzU32
    , at48kHzU32
    , at16kHzU64
    , at48kHzU64
    , nominalDiffTime
    , convertTicks
    , deriveFrameTimestamp
    , IsClock(..)
    , timeSince
    , UtcClock(..)
    , useUtcClock
    , _utcClockTimeDiff
    , _utcClockTime
    , utcClockTimeDiff
    , STicksK(..)
    , type STicksGetRate
    , type STicksGetTicks
    , type WithSTicks
    , HasStaticDuration(..)
    , toSTicksProxy
    , getStaticDuration
    , getStaticTicks
    , getStaticRate
    , ticksFromSTicks
    , rateFromSTicks
    ) where

import           Conduit
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Default
import           Data.Function                   ( on )
import           Data.Kind
import           Data.MediaBus.Monotone
import           Data.MediaBus.Series
import           Data.Proxy
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Word
import           GHC.TypeLits
import           Test.QuickCheck
import           GHC.Generics                    ( Generic )
import           Control.DeepSeq
import           System.Random
import           Data.Tagged

newtype Ticks rate w = MkTicks { _ticks :: w }
    deriving (Eq, Real, Integral, Enum, LocalOrd, Num, Arbitrary, Default, Generic, Random)

instance NFData w =>
         NFData (Ticks rate w)

mkTicks :: forall proxy rate baseType.
        proxy '(rate, baseType)
        -> baseType
        -> Ticks rate baseType
mkTicks _ = MkTicks

at8kHzU32 :: Proxy '(8000, Word32)
at8kHzU32 = Proxy

at16kHzU32 :: Proxy '(16000, Word32)
at16kHzU32 = Proxy

at48kHzU32 :: Proxy '(48000, Word32)
at48kHzU32 = Proxy

at16kHzU64 :: Proxy '(16000, Word64)
at16kHzU64 = Proxy

at48kHzU64 :: Proxy '(48000, Word64)
at48kHzU64 = Proxy

convertTicks :: (Integral w, Integral w', KnownNat r, KnownNat r')
             => Ticks r w
             -> Ticks r' w'
convertTicks = view (from nominalDiffTime) . view nominalDiffTime

nominalDiffTime :: forall r w.
                (Integral w, KnownNat r)
                => Iso' (Ticks r w) NominalDiffTime
nominalDiffTime = iso (toNDT . _ticks) (MkTicks . fromNDT)
  where
    toNDT = (/ rate) . fromIntegral
    fromNDT = round . (* rate)
    rate = fromInteger $ natVal (Proxy :: Proxy r)

instance (KnownNat r, Integral w, Show w) =>
         Show (Ticks r w) where
    show tix@(MkTicks x) = "(" ++
        show (view nominalDiffTime tix) ++
            ", " ++
                show x ++
                    "@" ++
                        show (natVal (Proxy :: Proxy r)) ++
                            "Hz)"

instance (Eq w, LocalOrd w) =>
         Ord (Ticks rate w) where
    (<=) = flip succeeds

-- | Types with a duration (e.g. audio samples).
class HasDuration a where
    getDuration :: a -> NominalDiffTime
    getDuration !x = from nominalDiffTime #
        (getDurationTicks x :: Ticks 1000000000000 Integer)
    getDurationTicks :: (Integral i, KnownNat r) => a -> Ticks r i
    getDurationTicks !x = nominalDiffTime # getDuration x

-- TODO rename *Timestamp to *Tick
class SetTimestamp t (GetTimestamp t) ~ t =>
      HasTimestampT t where
    type GetTimestamp t
    type SetTimestamp t s

class HasTimestampT t -- TODO inline HasTimestampT again
       =>
      HasTimestamp t where
    timestamp :: Lens t (SetTimestamp t s) (GetTimestamp t) s
    timestamp' :: Lens' t (GetTimestamp t)
    timestamp' = timestamp

instance (HasTimestampT a, HasTimestampT b, GetTimestamp a ~ GetTimestamp b) =>
         HasTimestampT (Series a b) where
    type GetTimestamp (Series a b) = GetTimestamp a
    type SetTimestamp (Series a b) t = Series (SetTimestamp a t) (SetTimestamp b t)

instance (HasTimestamp a, HasTimestamp b, GetTimestamp a ~ GetTimestamp b) =>
         HasTimestamp (Series a b) where
    timestamp f (Start a) = Start <$> timestamp f a
    timestamp f (Next b) = Next <$> timestamp f b

-- * Media Data Synchronization
deriveFrameTimestamp :: (Monad m, KnownNat r, Integral t, HasDuration a, HasTimestamp a)
                     => Ticks r t
                     -> Conduit a m (SetTimestamp a (Ticks r t))
deriveFrameTimestamp t0 =
    evalStateC t0 (awaitForever yieldSync)
  where
    yieldSync sb = do
        t <- get
        modify (+ (nominalDiffTime # getDuration sb))
        yield (sb & timestamp .~ t)

-- | Clocks can generate reference times, and they can convert these to tickss. Tickss are mere integrals
class (Default (ClockTimeDiff c), Ord (ClockTimeDiff c), Eq (ClockTimeDiff c), Num (ClockTimeDiff c), Show (ClockTime c), Eq (ClockTime c), Show (ClockTimeDiff c), LocalOrd (ClockTimeDiff c)) =>
      IsClock c where
    data ClockTime c
    data ClockTimeDiff c
    type MonadClock c (m :: Type -> Type) :: Constraint
    now :: MonadClock c m => m (ClockTime c)
    timeAsTimeDiff :: ClockTime c -> ClockTimeDiff c
    diffTime :: ClockTime c -> ClockTime c -> ClockTimeDiff c
    timeAddTimeDiff :: ClockTime c -> ClockTimeDiff c -> ClockTime c

timeSince :: (IsClock c, MonadClock c m, Monad m) => ClockTime c -> m (ClockTimeDiff c)
timeSince t0 = do
    t1 <- now
    return (diffTime t1 t0)

data UtcClock = MkUtcClock
    deriving Generic

instance NFData UtcClock

useUtcClock :: Proxy UtcClock
useUtcClock = Proxy

instance IsClock UtcClock where
    newtype ClockTime UtcClock = MkUtcClockTime{_utcClockTime :: UTCTime}
                      deriving (Eq, Generic)
    newtype ClockTimeDiff UtcClock = MkUtcClockTimeDiff{_utcClockTimeDiff ::
                                          NominalDiffTime}
                          deriving (Ord, Eq, Num, Generic)
    type MonadClock UtcClock m = MonadIO m
    now = MkUtcClockTime <$> liftIO getCurrentTime
    timeAsTimeDiff (MkUtcClockTime ref) =
        MkUtcClockTimeDiff $ diffUTCTime ref $ UTCTime (toEnum 0) 0
    timeAddTimeDiff (MkUtcClockTime t) (MkUtcClockTimeDiff dt) =
        MkUtcClockTime (addUTCTime dt t)
    diffTime (MkUtcClockTime later) (MkUtcClockTime sooner) =
        MkUtcClockTimeDiff $ diffUTCTime later sooner

instance NFData (ClockTime UtcClock)

instance Show (ClockTime UtcClock) where
    show (MkUtcClockTime t) = show t

instance Show (ClockTimeDiff UtcClock) where
    show (MkUtcClockTimeDiff t) =
        "dt:" ++ show t

instance NFData (ClockTimeDiff UtcClock)

instance Default (ClockTimeDiff UtcClock) where
    def = MkUtcClockTimeDiff $ fromInteger def

instance Arbitrary (ClockTime UtcClock) where
    arbitrary = MkUtcClockTime <$> (UTCTime <$> (ModifiedJulianDay <$> arbitrary)
                                       <*> (fromInteger <$> arbitrary))

instance Arbitrary (ClockTimeDiff UtcClock) where
    arbitrary = MkUtcClockTimeDiff . fromInteger <$> arbitrary

utcClockTimeDiff :: Lens' (ClockTimeDiff UtcClock) NominalDiffTime
utcClockTimeDiff = lens _utcClockTimeDiff (const MkUtcClockTimeDiff)

instance LocalOrd (ClockTimeDiff UtcClock) where
    succeeds = succeeds `on` roundToSeconds
      where
        roundToSeconds = round . (/ 1000000000000) . _utcClockTimeDiff
        roundToSeconds :: ClockTimeDiff UtcClock -> Word64

data STicksK = STicks Nat Nat

type family STicksGetRate (s :: STicksK) :: Nat where
        STicksGetRate ('STicks r t) = r

type family STicksGetTicks (s :: STicksK) :: Nat where
        STicksGetTicks ('STicks r t) = t

type WithSTicks r t x = Tagged ('STicks r t) x

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
getStaticDuration px = from nominalDiffTime # (ticksFromSTicks (toSTicksProxy px) :: Ticks r Integer)

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
