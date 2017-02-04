module Data.MediaBus.Clock
    ( HasDuration(..)
    , UtcClock(..)
    , utcTimeDiff
    , _utcTimeDiff
    , _utcTime
    , IsClock(..)
    , getClockRate
    , IsTiming(..)
    , Timing(..)
    , Sync(..)
    , syncContent
    , syncTimestamp
    , syncReference
    , synchronizeToClock
    , convertClockRate
    , convertTicks
    , adaptTimingSync
    , deriveFrameTimestamp
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
    ) where

import           Conduit
import           Data.Time.Clock
import           GHC.TypeLits
import           Data.Proxy
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Function                   ( on )
import           Data.MediaBus.Internal.Monotone
import           Data.Word
import           Data.Kind

-- | Types with an integral duration, i.e. a duration that corresponds to an
-- integral number of sub-units (e.g. audio samples).
class HasDuration a where
    getIntegralDuration :: Integral b => a -> b

class (KnownNat (GetClockRate t), SetClockRate t (GetClockRate t) ~ t) =>
      IsTiming t where
    type GetClockRate t :: Nat
    type SetClockRate t (n :: Nat)
    data Ticks t
    nominalDiffTime :: Iso' (Ticks t) NominalDiffTime

convertTicks :: (Num (Ticks t'), IsTiming t, IsTiming t') => Ticks t -> Ticks t'
convertTicks = (\t -> set nominalDiffTime t 0) . view nominalDiffTime

data Timing (rate :: Nat) (w :: Type) = MkTiming

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
                              deriving (Eq, Real, Integral, Enum, IsMonotone, Num)
    nominalDiffTime = iso (toNDT . _ticks) (MkTicks . fromNDT)
      where
        toNDT = (* rate) . fromIntegral
        fromNDT = round . (/ rate)
        rate = fromInteger $ natVal (Proxy :: Proxy rate)

instance (KnownNat r, Show w) =>
         Show (Ticks (Timing r w)) where
    show tix@(MkTicks x) = show x ++ "@" ++ show (getClockRate tix) ++ "Hz"

instance (Eq w, IsMonotone w) =>
         Ord (Ticks (Timing rate w)) where
    (<=) = flip succeeds

getClockRate :: forall c proxy.
             (KnownNat (GetClockRate c))
             => proxy c
             -> Integer
getClockRate _ = natVal (Proxy :: Proxy (GetClockRate c))

convertClockRate :: forall rIn rOut wIn wOut.
                 (IsTiming (Timing rIn wIn), IsTiming (Timing rOut wOut), Num wOut, Integral wIn)
                 => Ticks (Timing rIn wIn)
                 -> Ticks (Timing rOut wOut)
convertClockRate (MkTicks ticksIn) =
    let rateIn = natVal (Proxy :: Proxy rIn)
        rateOut = natVal (Proxy :: Proxy rOut)
        ticksOut = MkTicks (fromInteger ((rateIn * toInteger ticksIn) `div`
                                             rateOut))
    in
        ticksOut

-- * Media Data Synchronization
data Sync ref ts p = ReSync { _syncReference       :: ref
                            , _syncTimestamp :: ts
                            , _syncContent   :: p
                            }
                   | InSync { _syncTimestamp :: ts
                            , _syncContent   :: p
                            }
    deriving (Eq, Ord)

makeLenses ''Sync

instance (Show ref, Show ts, Show p) =>
         Show (Sync ref ts p) where
    show (ReSync ref ts p) =
        "RESYNC: " ++ show ref ++ " " ++ show ts ++ " @@ " ++ show p
    show (InSync ts p) = "SYNC: " ++ show ts ++ " @@ " ++ show p

instance Functor (Sync ref ts) where
    fmap = over syncContent

instance (IsMonotone t) =>
         IsMonotone (Sync r t p) where
    succeeds = succeeds `on` view syncTimestamp

adaptTimingSync :: (Num (Ticks t'), IsTiming t, IsTiming t')
                => Sync (Reference (Ticks t)) (Ticks t) a
                -> Sync (Reference (Ticks t')) (Ticks t') a
adaptTimingSync (ReSync (MkReference r) t a) =
    ReSync (MkReference (convertTicks r)) (convertTicks t) a
adaptTimingSync (InSync t a) =
    InSync (convertTicks t) a

deriveFrameTimestamp :: (Monad m, Integral (Ticks t), HasDuration a)
                     => Reference (Ticks t)
                     -> Conduit a m (Sync (Reference (Ticks t)) (Ticks t) a)
deriveFrameTimestamp ref@(MkReference t0) =
    evalStateC t0 go
  where
    go = do
        msb <- await
        maybe (return ())
              (\sb -> do
                   modify (+ getIntegralDuration sb)
                   yield (ReSync ref t0 sb)
                   awaitForever sendSyncedLoop)
              msb
      where
        sendSyncedLoop sb = do
            t <- get
            modify (+ getIntegralDuration sb)
            yield (InSync t sb)

-- | Clocks can generate reference times, and they can convert these to tickss. Tickss are mere integrals
class (Show (Time c), Show (TimeDiff c), IsMonotone (TimeDiff c)) =>
      IsClock c m where
    data Time c
    data TimeDiff c
    now :: m (Time c)
    noTimeDiff :: m (TimeDiff c)
    timeAsTimeDiff :: Time c -> m (TimeDiff c)
    timeSince :: Time c -> TimeDiff c -> m (TimeDiff c)

data UtcClock = MkUtcClock

instance (MonadIO m) =>
         IsClock UtcClock m where
    newtype Time UtcClock = MkUtcTime{_utcTime :: UTCTime}
                      deriving (Show, Eq)
    newtype TimeDiff UtcClock = MkUtcTimeDiff{_utcTimeDiff ::
                                          NominalDiffTime}
                          deriving (Show, Ord, Eq, Num)
    now = MkUtcTime <$> liftIO getCurrentTime
    noTimeDiff = return (MkUtcTimeDiff 0)
    timeAsTimeDiff (MkUtcTime ref) =
        return $ MkUtcTimeDiff $ diffUTCTime ref $ UTCTime (toEnum 0) 0
    timeSince (MkUtcTime ref) _t0 = do
        (MkUtcTime ref') <- now
        return $ MkUtcTimeDiff $ diffUTCTime ref' ref

utcTimeDiff :: Lens' (TimeDiff UtcClock) NominalDiffTime
utcTimeDiff = lens _utcTimeDiff (const MkUtcTimeDiff)

instance IsMonotone (TimeDiff UtcClock) where
    succeeds = succeeds `on` roundToSeconds
      where
        roundToSeconds = round . (/ 1000000000000) . _utcTimeDiff
        roundToSeconds :: TimeDiff UtcClock -> Word64

synchronizeToClock :: (IsClock c m, Monad m)
                   => Time c
                   -> Conduit a m (Sync (Time c) (TimeDiff c) a)
synchronizeToClock initialTime =
    let startState = (initialTime, Nothing)
    in
        evalStateC startState (awaitForever (lift . handle >=> yield))
  where
    handle p = do
        (startTime, mTicks) <- get
        nextT <- lift (maybe (timeAsTimeDiff initialTime)
                             (timeSince startTime)
                             mTicks)
        let sync = (case mTicks of
                        Nothing -> ReSync startTime
                        Just _ -> InSync) nextT
                                          p
        put (startTime, Just nextT)
        return sync
