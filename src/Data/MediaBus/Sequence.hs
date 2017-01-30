module Data.MediaBus.Sequence
    ( SeqNum(..)
    , fromSeqNum
    , SeqNumStart(..)
    , sequenceStart
    , SeqNumOf(..)
    , reorder
    , synchronizeToSeqNum
    , Discontinous(..)
    ) where

import qualified Data.Set                        as Set
import           Test.QuickCheck                 ( Arbitrary(..) )
import           Conduit
import           Data.MediaBus.Clock
import           Data.MediaBus.Internal.Monotone
import           Control.Lens
import           Data.Function                   ( on )
import           System.Random

newtype SeqNum a = MkSeqNum { _fromSeqNum :: a }
    deriving (Show, Num, Eq, Integral, Real, Bounded, Enum, IsMonotone, Arbitrary)

instance HasTimestamp (SeqNum a) where
    type GetTimestamp (SeqNum a) = a
    type SetTimestamp (SeqNum a) b = SeqNum b
    timestamp = iso _fromSeqNum MkSeqNum

instance (Eq a, IsMonotone a) =>
         Ord (SeqNum a) where
    compare x y
        | x == y = EQ
        | x `succeeds` y = GT
        | otherwise = LT

makeLenses ''SeqNum

-- | An offset, e.g. like the first rndom RTP timestamp to which the following
-- timestamps relate.
newtype SeqNumStart s = MkSeqNumStart { _sequenceStart :: s }
    deriving (Bounded, Integral, Num, Enum, Real, Ord, Eq, Arbitrary, Functor, IsMonotone, Random)

makeLenses ''SeqNumStart

instance Show s =>
         Show (SeqNumStart s) where
    show (MkSeqNumStart x) =
        "(+|" ++ show x ++ "|)"

-- | A pseudo clock that runs sequence numbers, starting from a random start
-- value.
-- | Frame some input to a sequence number.
data SeqNumOf s = MkSeqNumOf

instance (Num s, Applicative m) =>
         IsClock (SeqNumOf s) m where
    type ReferenceTime (SeqNumOf s) = SeqNumStart s
    type Timestamp (SeqNumOf s) = SeqNum s
    referenceTime _ = pure 0
    nextTimestamp _ _ t0 = pure (t0 + 1)
    zeroTimestamp _ = pure 0
    referenceTimestamp _ (MkSeqNumStart ref) =
        pure (MkSeqNum ref)

synchronizeToSeqNum :: (Monad m, Integral i)
                    => SeqNumOf i
                    -> SeqNumStart i
                    -> ConduitM a (SynchronizedTo (SeqNumStart i) (Event (SeqNum i) a)) m ()
synchronizeToSeqNum = synchronizeToClock

-- | Buffer incoming samples in a queue of the given size and output them in
-- order. The output is monotone increasing.
reorder :: (Eq (GetTimestamp a), IsMonotone (GetTimestamp a), HasTimestamp a, Monad m)
        => Int
        -> Conduit a m a
reorder windowSize = go Set.empty Nothing
  where
    go queue minIndex = do
        mx <- await
        case mx of
            Nothing -> mapM_ (yield . keyOrderedValue) queue
            Just x -> let xt = x ^. timestamp
                      in
                          if maybe False (not . succeeds xt) minIndex
                          then go queue minIndex
                          else let queue' = Set.insert (MkKeyOrdered (MkSeqNum xt)
                                                                     x)
                                                       queue
                                   mMinView = Set.minView queue'
                               in
                                   if Set.size queue >= windowSize
                                   then mapM_ (yield . keyOrderedValue . fst)
                                              mMinView
                                       >> go (maybe queue' snd mMinView)
                                             (maybe minIndex
                                                    (Just .
                                                         _fromSeqNum .
                                                             keyOrderedKey .
                                                                 fst)
                                                    mMinView)
                                   else go queue' minIndex

-- | Internal newtype wrapper 'Ord'ered on the key
data KeyOrdered k v = MkKeyOrdered { keyOrderedKey   :: k
                                   , keyOrderedValue :: v
                                   }
    deriving Show

instance Ord k =>
         Ord (KeyOrdered k v) where
    compare = compare `on` keyOrderedKey

instance Eq k =>
         Eq (KeyOrdered k v) where
    (==) = (==) `on` keyOrderedKey

-- -----------------------------------------------------
-- * Dealing with gaps in media streams
-- -----------------------------------------------------
-- | Differentiate between continuous and non-continous occurences of something.
data Discontinous a b = Gap a
                      | Continue b
    deriving (Show)

instance Functor (Discontinous a) where
    fmap f (Continue x) = Continue (f x)
    fmap _f (Gap x) = Gap x
