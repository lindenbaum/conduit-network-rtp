module Data.MediaBus.Clock
    ( UtcClock(..)
    , StaticUtcClock(..)
    , IsClock(..)
    , HasClock(..)
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

class SetClock s (GetClock s) ~ s =>
      HasClock s where
    type GetClock s
    type SetClock s t
    clock :: Lens s (SetClock s t) (GetClock s) t

-- | Clocks can generate reference times, and they can convert these to timestamps. Timestamps are mere integrals
class IsClock c m where
    data Timestamp c
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
    newtype Timestamp UtcClock = MkUtcTimestamp {utcTimestamp :: NominalDiffTime}
      deriving (Eq,Show,Ord,Num)
    referenceTime _ = liftIO getCurrentTime
    zeroTimestamp _ = return 0
    referenceTimestamp (MkClock res) ref =
        return (MkUtcTimestamp (diffUTCTime ref (UTCTime (toEnum 0) 0) / res))
    nextTimestamp clk@(MkClock res) ref _t0 = do
        ref' <- referenceTime clk
        return (MkUtcTimestamp (diffUTCTime ref' ref / res))

data StaticUtcClock (utcClockResolution :: Nat) = MkStaticUtcClock
    deriving (Show, Eq, Ord)

instance HasClock (StaticUtcClock u) where
    type GetClock (StaticUtcClock u) = StaticUtcClock u
    type SetClock (StaticUtcClock u) t = t
    clock = iso id id

instance (KnownNat res, MonadIO m) =>
         IsClock (StaticUtcClock res) m where
    type ReferenceTime (StaticUtcClock res) = UTCTime
    newtype Timestamp
          (StaticUtcClock res) = MkStaticUtcTimestamp{staticUtcTimestamp ::
                                                      NominalDiffTime}
                               deriving (Show, Ord, Eq)
    referenceTime _ = liftIO getCurrentTime
    zeroTimestamp _ = return (MkStaticUtcTimestamp 0)
    referenceTimestamp _ ref = do
        let res = 1 / fromInteger (natVal (Proxy :: Proxy res))
        return (MkStaticUtcTimestamp (diffUTCTime ref (UTCTime (toEnum 0) 0) /
                                          res))
    nextTimestamp clk ref _t0 = do
        let res = 1 / fromInteger (natVal (Proxy :: Proxy res))
        ref' <- referenceTime clk
        return (MkStaticUtcTimestamp (diffUTCTime ref' ref / res))

-- * Media Data Synchronization
data SynchronizedTo o p =
      SynchronizeTo { syncRef           :: o
                    , _fromSynchronized :: p
                    }
    | Synchronized { _fromSynchronized :: p }
    deriving (Eq)

makeLenses ''SynchronizedTo

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

instance HasTimestamp p =>
         HasTimestamp (SynchronizedTo o p) where
    type SetTimestamp (SynchronizedTo o p) t = SynchronizedTo o (SetTimestamp p t)
    type GetTimestamp (SynchronizedTo o p) = GetTimestamp p
    timestamp = fromSynchronized . timestamp

instance HasClock p =>
         HasClock (SynchronizedTo o p) where
    type SetClock (SynchronizedTo o p) c = SynchronizedTo o (SetClock p c)
    type GetClock (SynchronizedTo o p) = GetClock p
    clock = fromSynchronized . clock

instance (IsMonotone p) =>
         IsMonotone (SynchronizedTo o p) where
    succeeds = succeeds `on` _fromSynchronized

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
