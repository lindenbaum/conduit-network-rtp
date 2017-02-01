module Data.MediaBus.Clock
    ( UtcClock
    , ticks
    , IsClock(..)
    , HasTimestamp(..)
    , Event(..)
    , eventTimestamp
    , eventContent
    , SynchronizedTo(..)
    , fromSynchronized
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

-- | Clocks can generate reference times, and they can convert these to timestamps. Timestamps are mere integrals
class (Show (ReferenceTime c), Show (Timestamp c), KnownNat (GetSampleRate c)) =>
      IsClock c m where
    data Timestamp c -- TODO: rename to Ticks
    type ReferenceTime c -- TODO: rename to AbsoluteTime
    type GetSampleRate c :: Nat
    type SetSampleRate c (r :: Nat)
    referenceTime :: proxy c -> m (ReferenceTime c)
    zeroTimestamp :: proxy c -> m (Timestamp c)
    referenceTimestamp :: proxy c -> ReferenceTime c -> m (Timestamp c)
    nextTimestamp :: proxy c -> ReferenceTime c -> Timestamp c -> m (Timestamp c)

data UtcClock (sampleRate :: Nat)

instance (KnownNat clockFreq, MonadIO m) =>
         IsClock (UtcClock clockFreq) m where
    type ReferenceTime (UtcClock clockFreq) = UTCTime
    type GetSampleRate (UtcClock clockFreq) = clockFreq
    type SetSampleRate (UtcClock clockFreq) f = UtcClock f
    newtype Timestamp (UtcClock clockFreq) = MkTicks{_ticks ::
                                                 NominalDiffTime}
                                       deriving (Show, Ord, Eq, Num)
    referenceTime _ = liftIO getCurrentTime
    zeroTimestamp _ = return (MkTicks 0)
    referenceTimestamp _ ref = do
        let clockFreq = 1 / fromInteger (natVal (Proxy :: Proxy clockFreq))
        return (MkTicks (diffUTCTime ref (UTCTime (toEnum 0) 0) /
                             clockFreq))
    nextTimestamp clk ref _t0 = do
        let clockFreq = 1 / fromInteger (natVal (Proxy :: Proxy clockFreq))
        ref' <- referenceTime clk
        return (MkTicks (diffUTCTime ref' ref / clockFreq))

ticks :: Lens' (Timestamp (UtcClock r)) NominalDiffTime
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
data SynchronizedTo ref ts  p =
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
