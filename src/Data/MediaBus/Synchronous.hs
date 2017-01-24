module Data.MediaBus.Synchronous
    ( SynchronizedTo(..)
    , Offset(..)
    , addSequenceNumber
    , sampleWithUTC
    , addTimestamp
    , sampleWith
    ) where

import           Conduit
import           Data.Function              ( on )
import           Data.MediaBus.Ordered
import           Data.MediaBus.Basics
import           Test.QuickCheck            ( Arbitrary(..) )
import           Control.Monad.State.Strict
import           Data.Time.Clock            ( NominalDiffTime, UTCTime
                                            , diffUTCTime, getCurrentTime )

-- -----------------------------------------------------
-- * Media Data Synchronization
-- -----------------------------------------------------
-- | 'Event's for things that need to be initialized with a 'Clock' to
-- synchronize to.
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

-- | An offset, e.g. like the first rndom RTP timestamp to which the following
-- timestamps relate.
newtype Offset s = MkOffset { fromOffset :: s }
    deriving (Bounded, Integral, Num, Enum, Real, Ord, Eq, Arbitrary, Functor, IsMonotone)

instance Show s =>
         Show (Offset s) where
    show (MkOffset x) = "(+|" ++ show x ++ "|)"

-- | Sample some input to a sequence number.
addSequenceNumber :: (Monad m, Integral i)
                  => Offset i
                  -> ConduitM a (SynchronizedTo (Offset i) (Sample i a)) m ()
addSequenceNumber startValue =
    addTimestamp (pure startValue) handle
  where
    handle = do
        MkOffset s <- get
        modify (+ 1)
        return s

-- | Add 'UTCTime' timestamps as 'NominalDiffTime'
sampleWithUTC :: (MonadIO m)
              => Conduit a m (SynchronizedTo UTCTime (Sample NominalDiffTime a))
sampleWithUTC = addTimestamp start handle
  where
    start = liftIO getCurrentTime
    handle = do
        startTime <- get
        now <- liftIO getCurrentTime
        return (diffUTCTime now startTime)

-- | Synchronize a stream from a stateful callback.
addTimestamp :: Monad m
             => m s
             -> StateT s m t
             -> Conduit a m (SynchronizedTo s (Sample t a))
addTimestamp start handle =
    sampleWith start' handle'
  where
    start' = do
        c <- start
        return (True, c)
    handle' p = do
        (isFirst, c) <- get
        (t, cNext) <- lift (runStateT handle c)
        put (False, cNext)
        return ((if isFirst then SynchronizeTo c else Synchronized) (MkSample t
                                                                              p))

sampleWith :: Monad m
           => m s
           -> (a -> StateT s m (SynchronizedTo c (Sample t a)))
           -> Conduit a m (SynchronizedTo c (Sample t a))
sampleWith startStateM handle = do
    startState <- lift startStateM
    evalStateC startState (awaitForever (lift . handle >=> yield))
