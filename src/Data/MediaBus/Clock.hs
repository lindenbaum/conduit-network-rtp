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
    , _utcTimeDiff
    , _utcTime
    , utcTimeDiff
    , overwriteTimeInSeries
    ) where

import           Conduit
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Default
import           Data.Function                   ( on )
import           Data.Kind
import           Data.MediaBus.Internal.Monotone
import           Data.MediaBus.Internal.Series
import           Data.Proxy
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Word
import           GHC.TypeLits
import           Test.QuickCheck
import           Data.Functor

newtype Ticks rate w = MkTicks { _ticks :: w }
    deriving (Eq, Real, Integral, Enum, IsMonotone, Num, Arbitrary, Default)

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

instance (Eq w, IsMonotone w) =>
         Ord (Ticks rate w) where
    (<=) = flip succeeds

-- | Types with an integral duration, i.e. a duration that corresponds to an
-- integral number of sub-units (e.g. audio samples).
class HasDuration a where
    getDuration :: Integral b => a -> b

-- TODO rename *Timestamp to *Tick
class SetTimestamp t (GetTimestamp t) ~ t =>
      HasTimestampT t where
    type GetTimestamp t
    type SetTimestamp t s

class HasTimestampT t =>
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
deriveFrameTimestamp :: (Monad m, Integral (Ticks r t), HasDuration a, HasTimestamp a)
                     => Ticks r t
                     -> Conduit a m (SetTimestamp a (Ticks r t))
deriveFrameTimestamp t0 =
    evalStateC t0 (awaitForever yieldSync)
  where
    yieldSync sb = do
        t <- get
        modify (+ getDuration sb)
        yield (sb & timestamp .~ t)

-- | Clocks can generate reference times, and they can convert these to tickss. Tickss are mere integrals
class (Default (TimeDiff c), Ord (TimeDiff c), Eq (TimeDiff c), Num (TimeDiff c), Show (Time c), Eq (Time c), Show (TimeDiff c), IsMonotone (TimeDiff c)) =>
      IsClock c where
    data Time c
    data TimeDiff c
    type MonadClock c (m :: Type -> Type) :: Constraint
    now :: MonadClock c m => m (Time c)
    timeAsTimeDiff :: Time c -> TimeDiff c
    diffTime :: Time c -> Time c -> TimeDiff c
    timeAddTimeDiff :: Time c -> TimeDiff c -> Time c

timeSince :: (IsClock c, MonadClock c m, Monad m) => Time c -> m (TimeDiff c)
timeSince t0 = do
    t1 <- now
    return (diffTime t1 t0)

data UtcClock = MkUtcClock

useUtcClock :: Proxy UtcClock
useUtcClock = Proxy

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
    diffTime (MkUtcTime later) (MkUtcTime sooner) =
        MkUtcTimeDiff $ diffUTCTime later sooner

instance Show (Time UtcClock) where
    show (MkUtcTime t) = show t

instance Show (TimeDiff UtcClock) where
    show (MkUtcTimeDiff t) =
        "dt:" ++ show t

instance Default (TimeDiff UtcClock) where
    def = MkUtcTimeDiff $ fromInteger def

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

overwriteTimeInSeries :: (HasTimestamp a, HasTimestamp b, GetTimestamp b ~ TimeDiff c, GetTimestamp a ~ TimeDiff c, Monad m, IsClock c, MonadClock c m, AsSeries ser a b)
                      => proxy c
                      -> Conduit ser m ser
overwriteTimeInSeries _ = do
    tStart0 <- lift now
    evalStateC tStart0 (awaitForever wrapTime)
  where
    wrapTime ser = do
        tStart <- get
        tNow <- lift (lift now)
        ser' <- mapMOf (seriesStart' . timestamp')
                       (const (put tNow $> timeAsTimeDiff tNow))
                       ser
        yield (set (seriesNext' . timestamp') (diffTime tNow tStart) ser')
