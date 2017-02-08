module Data.MediaBus.Clock
    ( HasDuration(..)
    , IsTiming(..)
    , HasTimestamp(..)
    , Timing(..)
    , mkTicks
    , convertTicks
    , type At8kHzU32
    , at8kHzU32
    , type At16kHzU32
    , at16kHzU32
    , type At48kHzU32
    , at48kHzU32
    , type At8kHzU64
    , at8kHzU64
    , type At16kHzU64
    , at16kHzU64
    , type At48kHzU64
    , at48kHzU64
    , getClockRate
    , deriveFrameTimestamp
    , IsClock(..)
    , UtcClock(..)
    , _utcTimeDiff
    , _utcTime
    , utcTimeDiff
    , synchronizeToClock
    , type ClockSynced
    ) where

import           Conduit
import           Data.Time.Clock
import           GHC.TypeLits
import           Data.Proxy
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Function                   ( on )
import           Data.MediaBus.Internal.Monotone
import           Data.MediaBus.Internal.Series
import           Data.Word
import           Data.Kind
import           Test.QuickCheck
import           Data.Time.Calendar
import           Data.Default

-- | Types with an integral duration, i.e. a duration that corresponds to an
-- integral number of sub-units (e.g. audio samples).
class HasDuration a where
    getDuration :: Integral b => a -> b

class (KnownNat (GetClockRate t), SetClockRate t (GetClockRate t) ~ t) =>
      IsTiming t where
    type GetClockRate t :: Nat
    type SetClockRate t (n :: Nat)
    data Ticks t
    nominalDiffTime :: Iso' (Ticks t) NominalDiffTime

class SetTimestamp t (GetTimestamp t) ~ t =>
      HasTimestamp t where
    type GetTimestamp t
    type SetTimestamp t s
    timestamp :: Lens t (SetTimestamp t s) (GetTimestamp t) s

instance (HasTimestamp a, HasTimestamp b, GetTimestamp a ~ GetTimestamp b) =>
         HasTimestamp (Series a b) where
    type GetTimestamp (Series a b) = GetTimestamp a
    type SetTimestamp (Series a b) t = Series (SetTimestamp a t) (SetTimestamp b t)
    timestamp f (Start a) = Start <$> timestamp f a
    timestamp f (Next b) = Next <$> timestamp f b

data Timing (rate :: Nat) (w :: Type) = MkTiming

mkTicks :: Timing r w -> w -> Ticks (Timing r w)
mkTicks _ = MkTicks

convertTicks :: (IsTiming t, IsTiming t') => Ticks t -> Ticks t'
convertTicks = view (from nominalDiffTime) . view nominalDiffTime

type At8kHzU32 = Timing 8000 Word32

at8kHzU32 :: At8kHzU32
at8kHzU32 = MkTiming

type At16kHzU32 = Timing 16000 Word32

at16kHzU32 :: At16kHzU32
at16kHzU32 = MkTiming

type At48kHzU32 = Timing 48000 Word32

at48kHzU32 :: At48kHzU32
at48kHzU32 = MkTiming

type At8kHzU64 = Timing 8000 Word64

at8kHzU64 :: At8kHzU64
at8kHzU64 = MkTiming

type At16kHzU64 = Timing 16000 Word64

at16kHzU64 :: At16kHzU64
at16kHzU64 = MkTiming

type At48kHzU64 = Timing 48000 Word64

at48kHzU64 :: At48kHzU64
at48kHzU64 = MkTiming

instance (Integral w, KnownNat rate) =>
         IsTiming (Timing rate w) where
    type GetClockRate (Timing rate w) = rate
    type SetClockRate (Timing rate w) rate' = Timing rate' w
    newtype Ticks (Timing rate w) = MkTicks{_ticks :: w}
                              deriving (Eq, Real, Integral, Enum, IsMonotone, Num, Arbitrary,
                                        Default)
    nominalDiffTime = iso (toNDT . _ticks) (MkTicks . fromNDT)
      where
        toNDT = (/ rate) . fromIntegral
        fromNDT = round . (* rate)
        rate = fromInteger $ natVal (Proxy :: Proxy rate)

instance (KnownNat r, Integral w, Show w) =>
         Show (Ticks (Timing r w)) where
    show tix@(MkTicks x) = "(" ++
        show (view nominalDiffTime tix) ++
            ", " ++
                show x ++ "@" ++ show (getClockRate tix) ++ "Hz)"

instance (Eq w, IsMonotone w) =>
         Ord (Ticks (Timing rate w)) where
    (<=) = flip succeeds

getClockRate :: forall c proxy.
             (KnownNat (GetClockRate c))
             => proxy c
             -> Integer
getClockRate _ = natVal (Proxy :: Proxy (GetClockRate c))

-- * Media Data Synchronization
deriveFrameTimestamp :: (Monad m, Integral (Ticks t), HasDuration a, HasTimestamp a)
                     => Ticks t
                     -> Conduit a m (SetTimestamp a (Ticks t))
deriveFrameTimestamp t0 =
    evalStateC t0 (awaitForever yieldSync)
  where
    yieldSync sb = do
        t <- get
        modify (+ getDuration sb)
        yield (sb & timestamp .~ t)

-- | Clocks can generate reference times, and they can convert these to tickss. Tickss are mere integrals
class (Ord (TimeDiff c), Eq (TimeDiff c), Num (TimeDiff c), Show (Time c), Eq (Time c), Show (TimeDiff c), IsMonotone (TimeDiff c)) =>
      IsClock c where
    data Time c
    data TimeDiff c
    type MonadClock c (m :: Type -> Type) :: Constraint
    now :: MonadClock c m => m (Time c)
    timeAsTimeDiff :: Time c -> TimeDiff c
    timeAddTimeDiff :: Time c -> TimeDiff c -> Time c
    timeSince :: MonadClock c m => Time c -> m (TimeDiff c)

data UtcClock = MkUtcClock

instance IsClock UtcClock where
    newtype Time UtcClock = MkUtcTime{_utcTime :: UTCTime}
                      deriving Eq
    newtype TimeDiff UtcClock = MkUtcTimeDiff{_utcTimeDiff ::
                                          NominalDiffTime}
                          deriving (Ord, Eq, Num)
    type MonadClock UtcClock m = MonadIO m
    now = MkUtcTime <$> liftIO getCurrentTime
    timeAsTimeDiff (MkUtcTime ref) =
        MkUtcTimeDiff $ diffUTCTime ref $ UTCTime (toEnum 0) 0
    timeAddTimeDiff (MkUtcTime t) (MkUtcTimeDiff dt) =
        MkUtcTime (addUTCTime dt t)
    timeSince (MkUtcTime ref) = do
        (MkUtcTime ref') <- now
        return $ MkUtcTimeDiff $ diffUTCTime ref' ref

instance Show (Time UtcClock) where
    show (MkUtcTime t) = show t

instance Show (TimeDiff UtcClock) where
    show (MkUtcTimeDiff t) =
        "dt:" ++ show t

instance Arbitrary (Time UtcClock) where
    arbitrary = MkUtcTime <$> (UTCTime <$> (ModifiedJulianDay <$> arbitrary)
                                       <*> (fromInteger <$> arbitrary))

instance Arbitrary (TimeDiff UtcClock) where
    arbitrary = MkUtcTimeDiff . fromInteger <$> arbitrary

utcTimeDiff :: Lens' (TimeDiff UtcClock) NominalDiffTime
utcTimeDiff = lens _utcTimeDiff (const MkUtcTimeDiff)

instance IsMonotone (TimeDiff UtcClock) where
    succeeds = succeeds `on` roundToSeconds
      where
        roundToSeconds = round . (/ 1000000000000) . _utcTimeDiff
        roundToSeconds :: TimeDiff UtcClock -> Word64

type ClockSynced c a = Series (Time c) (TimeDiff c, a)

synchronizeToClock :: (IsClock c, Monad m, MonadClock c m)
                   => Conduit i m (ClockSynced c i)
synchronizeToClock = do
    startTime <- lift now
    monotoneSeriesC (return startTime) (wrapTimeDiff startTime)
  where
    wrapTimeDiff startTime i = do
        ts <- timeSince startTime
        return (ts, i)
