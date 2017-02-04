module Data.MediaBus.Sequence
    ( SeqNum(..)
    , fromSeqNum
    , reorder
    , synchronizeToSeqNum
    , toSeqNum
    , HasSeqNum(..)
    , Discontinous(..)
    ) where

import           Control.Monad.State.Strict      ( gets )
import qualified Data.Set                        as Set
import           Test.QuickCheck                 ( Arbitrary(..) )
import           Conduit
import           Data.MediaBus.Clock
import           Data.MediaBus.Internal.Monotone
import           Control.Lens
import           Data.Function                   ( on )

class (Eq (GetSeqNum s), Show (GetSeqNum s), IsMonotone (GetSeqNum s), Ord (GetSeqNum s), Num (GetSeqNum s), s ~ SetSeqNum s (GetSeqNum s)) =>
      HasSeqNum s where
    type GetSeqNum s
    type SetSeqNum s t
    sequenceNumber :: Lens s (SetSeqNum s t) (GetSeqNum s) t

instance (Num ts, Eq ts, Show ts, IsMonotone ts, Ord ts) =>
         HasSeqNum (Sync ref ts p) where
    type GetSeqNum (Sync ref ts p) = ts
    type SetSeqNum (Sync ref ts p) ts' = Sync ref ts' p
    sequenceNumber = syncTimestamp

newtype SeqNum s = MkSeqNum { _fromSeqNum :: s }
    deriving (Show, Num, Eq, Bounded, Enum, IsMonotone, Arbitrary)

makeLenses ''SeqNum

instance (Num ts, Eq ts, Show ts, IsMonotone ts, Ord ts) =>
         HasSeqNum (SeqNum ts) where
    type GetSeqNum (SeqNum s) = s
    type SetSeqNum (SeqNum s) t = SeqNum t
    sequenceNumber = fromSeqNum

instance (Eq a, IsMonotone a) =>
         Ord (SeqNum a) where
    compare x y
        | x == y = EQ
        | x `succeeds` y = GT
        | otherwise = LT

deriving instance (Real a, Num a, Eq a, IsMonotone a) => Real
         (SeqNum a)

deriving instance
         (Integral a, Enum a, Real a, Eq a, IsMonotone a) => Integral
         (SeqNum a)

toSeqNum :: Integral s => Reference s -> SeqNum s
toSeqNum = fromIntegral

synchronizeToSeqNum :: (Monad m, Integral i)
                    => Reference i
                    -> ConduitM a (Sync (Reference i) (SeqNum i) a) m ()
synchronizeToSeqNum startSeq =
    evalStateC (startSeq, fromIntegral startSeq) $ do
        sendReSync
        awaitForever sendInSync
  where
    sendReSync = do
        ma <- await
        nextSeq <- gets snd
        maybe (return ()) (yield . ReSync startSeq nextSeq) ma
        _2 %= (+ 1)
    sendInSync a = do
        nextSeq <- _2 <%= (+ 1)
        yield (InSync nextSeq a)

-- | Buffer incoming samples in a queue of the given size and output them in
-- order. The output is monotone increasing.
reorder :: (HasSeqNum a, Monad m) => Int -> Conduit a m a
reorder windowSize = go Set.empty Nothing
  where
    go queue minIndex = do
        mx <- await
        case mx of
            Nothing -> mapM_ (yield . keyOrderedValue) queue
            Just x -> let xt = x ^. sequenceNumber
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
