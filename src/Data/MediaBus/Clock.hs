module Data.MediaBus.Clock
    ( UtcClock(..)
    , ticks
    , IsClock(..)
    , getClockRate
    , HasTimestamp(..)
    , Event(..)
    , eventTimestamp
    , eventContent
    , SynchronizedTo(..)
    , fromSynchronized
    , synchronizeToClock
    , type GetClock
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

-- | A type class for things that have a time stamp
class SetTimestamp s (GetTimestamp s) ~ s =>
      HasTimestamp s where
    type SetTimestamp s t
    type GetTimestamp s
    timestamp :: Lens s (SetTimestamp s t) (GetTimestamp s) t
    timestamp' :: Lens' s (GetTimestamp s)
    timestamp' = timestamp

instance HasTimestamp NominalDiffTime where
    type GetTimestamp NominalDiffTime = NominalDiffTime
    type SetTimestamp NominalDiffTime t = t
    timestamp = iso id id

class (KnownNat (GetRate t), SetRate t (GetRate t) ~ t) =>
      IsTiming t where
    type GetRate t :: Nat
    type SetRate t (n :: Nat)
    data Ticks t
    nominalDiffTime :: Iso' NominalDiffTime (Ticks t)

data Timing (rate :: Nat) (w :: Type) = MkTiming

instance (Integral w, KnownNat rate, IsMonotone w, Num w, Show w) =>
         IsTiming (Timing rate w) where
    type GetRate (Timing rate w) = rate
    type SetRate (Timing rate w) rate' = Timing rate'
    data Ticks (Timing rate w) = MkTicks{fromTicks :: w}
    nominalDiffTime = iso (MkTicks . fromNDT) (toNDT . fromTicks)
      where
        toNDT = (* rate) . fromIntegral
        fromNDT = round . (/ rate)
        rate = fromInteger $ natVal (Proxy :: Proxy rate)

-- | Clocks can generate reference times, and they can convert these to timestamps. Timestamps are mere integrals
class (Show (ReferenceTime c), Show (Timestamp c), KnownNat (GetSampleRate c), IsMonotone (Timestamp c)) =>
      IsClock c m where
    data Timestamp c -- TODO: rename to Ticks
    type ReferenceTime c -- TODO: rename to AbsoluteTime
    type GetSampleRate c :: Nat
    type SetSampleRate c (r :: Nat)
    referenceTime :: proxy c -> m (ReferenceTime c)
    zeroTimestamp :: proxy c -> m (Timestamp c)
    referenceTimestamp :: proxy c -> ReferenceTime c -> m (Timestamp c)
    nextTimestamp :: proxy c
                  -> ReferenceTime c
                  -> Timestamp c
                  -> m (Timestamp c)
    referenceTimeAtNewRate :: (IsClock (SetSampleRate c n) m, KnownNat n)
                           => proxy1 c
                           -> proxy2 n
                           -> ReferenceTime c
                           -> m (ReferenceTime (SetSampleRate c n))
    timestampAtNewRate :: (IsClock (SetSampleRate c n) m, KnownNat n)
                       => proxy1 c
                       -> proxy2 n
                       -> Timestamp c
                       -> m (Timestamp (SetSampleRate c n))

getClockRate :: forall c proxy.
             (KnownNat (GetSampleRate c))
             => proxy c
             -> Integer
getClockRate _ = natVal (Proxy :: Proxy (GetSampleRate c))

data UtcClock (sampleRate :: Nat) = MkUtcClock

instance (KnownNat clockFreq, MonadIO m) =>
         IsClock (UtcClock clockFreq) m where
    type ReferenceTime (UtcClock clockFreq) = UTCTime
    type GetSampleRate (UtcClock clockFreq) = clockFreq
    type SetSampleRate (UtcClock clockFreq) f = UtcClock f
    newtype Timestamp (UtcClock clockFreq) = MkTicks{_ticks :: Word64}
                                       deriving (Show, Ord, Eq, Num, IsMonotone)
    referenceTime _ = liftIO getCurrentTime
    zeroTimestamp _ = return (MkTicks 0)
    referenceTimestamp _ ref = do
        let clockFreq = fromInteger (natVal (Proxy :: Proxy clockFreq))
        return (MkTicks (round (diffUTCTime ref (UTCTime (toEnum 0) 0) *
                                    clockFreq)))
    nextTimestamp clk ref _t0 = do
        let clockFreq = fromInteger (natVal (Proxy :: Proxy clockFreq))
        ref' <- referenceTime clk
        return (MkTicks (round (diffUTCTime ref' ref * clockFreq)))
    referenceTimeAtNewRate _clk _newRate =
        return
    timestampAtNewRate clk newRate (MkTicks ticksIn) = do
        let rateIn = getClockRate clk
            rateOut = natVal newRate
            ticksOut = MkTicks (fromInteger ((rateIn * toInteger ticksIn) `div`
                                                 rateOut))
        return ticksOut

ticks :: Lens' (Timestamp (UtcClock r)) Word64
ticks = iso _ticks MkTicks

data Event t b = MkEvent { _eventTimestamp :: t
                         , _eventContent   :: b
                         }

instance (IsMonotone t) =>
         IsMonotone (Event t p) where
    succeeds = succeeds `on` _eventTimestamp

makeLenses ''Event

instance HasTimestamp (Event t b) where
    type SetTimestamp (Event t b) t' = (Event t' b)
    type GetTimestamp (Event t b) = t
    timestamp = eventTimestamp

instance (Show t, Show b) =>
         Show (Event t b) where
    show (MkEvent t b) = "<@ " ++
        show t ++ " - " ++ show b ++ " @>"

instance Eq t =>
         Eq (Event t b) where
    (==) = (==) `on` _eventTimestamp

instance Ord t =>
         Ord (Event t b) where
    compare = compare `on` _eventTimestamp

instance Functor (Event t) where
    fmap f (MkEvent t x) = MkEvent t (f x)

-- * Media Data Synchronization
data SynchronizedTo ref ts p =
      SynchronizeTo { syncRef           :: ref
                    , _fromSynchronized :: Event ts p
                    }
    | Synchronized { _fromSynchronized :: Event ts p }
    deriving (Eq)

makeLenses ''SynchronizedTo

instance (Show ref, Show ts, Show p) =>
         Show (SynchronizedTo ref ts p) where
    show (SynchronizeTo o p) =
        "/SyncTo: " ++ show o ++ "|" ++ show p ++ "/"
    show (Synchronized p) = "/" ++ show p ++ "/"

instance Functor (SynchronizedTo ref ts) where
    fmap f (SynchronizeTo o p) =
        SynchronizeTo o (fmap f p)
    fmap f (Synchronized p) =
        Synchronized (fmap f p)

instance HasTimestamp (SynchronizedTo r t p) where
    type SetTimestamp (SynchronizedTo r t p) t' = SynchronizedTo r t' p
    type GetTimestamp (SynchronizedTo r t p) = t
    timestamp = fromSynchronized . timestamp

type family GetClock t where
        GetClock (Timestamp c) = c

instance (IsMonotone t) =>
         IsMonotone (SynchronizedTo r t p) where
    succeeds = succeeds `on` view timestamp

synchronizeToClock :: (IsClock c m, Monad m)
                   => proxy c
                   -> ReferenceTime c
                   -> Conduit a m (SynchronizedTo (ReferenceTime c) (Timestamp c) a)
synchronizeToClock clk initialTime =
    let startState = (initialTime, Nothing)
    in
        evalStateC startState (awaitForever (lift . handle >=> yield))
  where
    handle p = do
        (startTime, mTimestamp) <- get
        nextT <- lift (maybe (referenceTimestamp clk initialTime)
                             (nextTimestamp clk startTime)
                             mTimestamp)
        let pOut = MkEvent nextT p
            sync = case mTimestamp of
                Nothing -> SynchronizeTo startTime pOut
                Just _ -> Synchronized pOut
        put (startTime, Just nextT)
        return sync
