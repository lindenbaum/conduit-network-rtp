module Data.MediaBus.Clock
    ( UtcClock(..)
    , IsClock(..)
    , HasClock(..)
    , HasTimestamp(..)
    , ClockRate(..)
    , Event(..)
    , eventTimestamp
    , eventContent
    , At8kHz
    , At12kHz
    , At16kHz
    , At22050Hz
    , At32kHz
    , At44100Hz
    , At48kHz
    , SynchronizedTo(..)
    , synchronizeToClock
    ) where

import           Conduit
import           Data.Time.Clock
import           GHC.TypeLits
import           Data.Proxy
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Function                   ( on )

import           Data.MediaBus.Internal.Monotone

-- | A type class for things that have a time stamp
class SetTimestamp s (GetTimestamp s) ~ s =>
      HasTimestamp s where
    type SetTimestamp s t
    type GetTimestamp s
    timestamp :: Lens s (SetTimestamp s t) (GetTimestamp s) t

instance HasTimestamp NominalDiffTime where
    type GetTimestamp NominalDiffTime = NominalDiffTime
    type SetTimestamp NominalDiffTime t = t
    timestamp = iso id id

class SetClock s (GetClock s) ~ s =>
      HasClock s where
    type GetClock s
    type SetClock s t
    clock :: Lens s (SetClock s t) (GetClock s) t

newtype ClockRate = MkClockRate Nat

type At8kHz = 'MkClockRate 8000

type At12kHz = 'MkClockRate 12000

type At16kHz = 'MkClockRate 16000

type At22050Hz = 'MkClockRate 22050

type At32kHz = 'MkClockRate 32000

type At44100Hz = 'MkClockRate 44100

type At48kHz = 'MkClockRate 48000

instance (KnownNat rate) =>
         HasClock (Proxy ('MkClockRate rate)) where
    type GetClock (Proxy ('MkClockRate rate)) = UtcClock
    type SetClock (Proxy ('MkClockRate rate)) t = Proxy ('MkClockRate rate)
    clock = lens (MkClock . fromInteger . natVal . getRate) const
      where
        getRate :: Proxy ('MkClockRate rate) -> Proxy rate
        getRate _ = Proxy

-- | Clocks can generate reference times, and they can convert these to timestamps. Timestamps are mere integrals
class IsClock c m where
    type Timestamp c
    type ReferenceTime c
    referenceTime :: c -> m (ReferenceTime c)
    zeroTimestamp :: c -> m (Timestamp c)
    referenceTimestamp :: c -> ReferenceTime c -> m (Timestamp c)
    nextTimestamp :: c -> ReferenceTime c -> Timestamp c -> m (Timestamp c)

newtype UtcClock = MkClock { utcClockResolution :: NominalDiffTime }
    deriving (Show, Eq, Ord)

instance HasClock UtcClock where
    type GetClock UtcClock = UtcClock
    type SetClock UtcClock t = t
    clock = iso id id

instance MonadIO m =>
         IsClock UtcClock m where
    type ReferenceTime UtcClock = UTCTime
    type Timestamp UtcClock = NominalDiffTime
    referenceTime _ = liftIO getCurrentTime
    zeroTimestamp _ = return 0
    referenceTimestamp (MkClock res) ref =
        return (diffUTCTime ref (UTCTime (toEnum 0) 0) / res)
    nextTimestamp clk@(MkClock res) ref _t0 = do
        ref' <- referenceTime clk
        return (diffUTCTime ref' ref / res)

-- * Media Data Synchronization
data SynchronizedTo o p =
      SynchronizeTo { syncRef          :: o
                    , fromSynchronized :: p
                    }
    | Synchronized { fromSynchronized :: p }
    deriving (Eq)

instance (Show o, Show p) =>
         Show (SynchronizedTo o p) where
    show (SynchronizeTo o p) =
        "/SyncTo: " ++ show o ++ "|" ++ show p ++ "/"
    show (Synchronized p) = "/" ++ show p ++ "/"

instance Functor (SynchronizedTo o) where
    fmap f (SynchronizeTo o p) =
        SynchronizeTo o (f p)
    fmap f (Synchronized p) =
        Synchronized (f p)

instance (IsMonotone p) =>
         IsMonotone (SynchronizedTo o p) where
    succeeds = succeeds `on` fromSynchronized

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

synchronizeToClock :: (IsClock c m, Monad m)
                   => c
                   -> ReferenceTime c
                   -> Conduit a m (SynchronizedTo (ReferenceTime c) (Event (Timestamp c) a))
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
