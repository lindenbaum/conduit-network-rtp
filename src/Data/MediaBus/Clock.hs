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
import           GHC.Generics                    ( Generic )
import           Control.DeepSeq
import           System.Random

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
    getDuration !x = from nominalDiffTime # (getDurationTicks x :: Ticks 1000000000000 Integer)
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
class (Default (TimeDiff c), Ord (TimeDiff c), Eq (TimeDiff c), Num (TimeDiff c), Show (Time c), Eq (Time c), Show (TimeDiff c), LocalOrd (TimeDiff c)) =>
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
    deriving Generic

instance NFData UtcClock

useUtcClock :: Proxy UtcClock
useUtcClock = Proxy

instance IsClock UtcClock where
    newtype Time UtcClock = MkUtcTime{_utcTime :: UTCTime}
                      deriving (Eq, Generic)
    newtype TimeDiff UtcClock = MkUtcTimeDiff{_utcTimeDiff ::
                                          NominalDiffTime}
                          deriving (Ord, Eq, Num, Generic)
    type MonadClock UtcClock m = MonadIO m
    now = MkUtcTime <$> liftIO getCurrentTime
    timeAsTimeDiff (MkUtcTime ref) =
        MkUtcTimeDiff $ diffUTCTime ref $ UTCTime (toEnum 0) 0
    timeAddTimeDiff (MkUtcTime t) (MkUtcTimeDiff dt) =
        MkUtcTime (addUTCTime dt t)
    diffTime (MkUtcTime later) (MkUtcTime sooner) =
        MkUtcTimeDiff $ diffUTCTime later sooner

instance NFData (Time UtcClock)

instance Show (Time UtcClock) where
    show (MkUtcTime t) = show t

instance Show (TimeDiff UtcClock) where
    show (MkUtcTimeDiff t) =
        "dt:" ++ show t

instance NFData (TimeDiff UtcClock)

instance Default (TimeDiff UtcClock) where
    def = MkUtcTimeDiff $ fromInteger def

instance Arbitrary (Time UtcClock) where
    arbitrary = MkUtcTime <$> (UTCTime <$> (ModifiedJulianDay <$> arbitrary)
                                       <*> (fromInteger <$> arbitrary))

instance Arbitrary (TimeDiff UtcClock) where
    arbitrary = MkUtcTimeDiff . fromInteger <$> arbitrary

utcTimeDiff :: Lens' (TimeDiff UtcClock) NominalDiffTime
utcTimeDiff = lens _utcTimeDiff (const MkUtcTimeDiff)

instance LocalOrd (TimeDiff UtcClock) where
    succeeds = succeeds `on` roundToSeconds
      where
        roundToSeconds = round . (/ 1000000000000) . _utcTimeDiff
        roundToSeconds :: TimeDiff UtcClock -> Word64
