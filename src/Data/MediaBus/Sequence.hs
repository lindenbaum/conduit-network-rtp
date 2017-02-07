module Data.MediaBus.Sequence
    ( SeqNum(..)
    , SequenceNumbered(..)
    , seqNum
    , seqNumValue
    , fromSeqNum
    , reorder
    , overSequenceNumberedC
    , synchronizeToSeqNum
    , HasSeqNum(..)
    , Discontinous(..)
    ) where

import qualified Data.Set                        as Set
import           Test.QuickCheck                 ( Arbitrary(..) )
import           Conduit
import           Data.MediaBus.Internal.Monotone
import           Control.Lens
import           Data.Function                   ( on )
import           Control.Monad.State

class (Eq (GetSeqNum s), Show (GetSeqNum s), IsMonotone (GetSeqNum s), Ord (GetSeqNum s), Num (GetSeqNum s), s ~ SetSeqNum s (GetSeqNum s)) =>
      HasSeqNum s where
    type GetSeqNum s
    type SetSeqNum s t
    sequenceNumber :: Lens s (SetSeqNum s t) (GetSeqNum s) t

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

data SequenceNumbered i a =
      MkSequenceNumbered { _seqNum      :: i
                         , _seqNumValue :: a
                         }
    deriving (Show, Eq)

makeLenses ''SequenceNumbered

instance Functor (SequenceNumbered i) where
    fmap = over seqNumValue

instance IsMonotone a =>
         IsMonotone (SequenceNumbered a p) where
    succeeds = succeeds `on` _seqNum

instance (Eq a, Eq p, IsMonotone a) =>
         Ord (SequenceNumbered a p) where
    compare x y
        | xi `succeeds` yi = GT
        | xi == yi = EQ
        | otherwise = LT
      where
        xi = _seqNum x
        yi = _seqNum y

instance (Num ts, Eq ts, Show ts, IsMonotone ts, Ord ts) =>
         HasSeqNum (SequenceNumbered ts a) where
    type GetSeqNum (SequenceNumbered ts a) = ts
    type SetSeqNum (SequenceNumbered ts a) t = SequenceNumbered t a
    sequenceNumber = seqNum

overSequenceNumberedC :: Monad m
                      => (i -> ConduitM a a' m ())
                      -> ConduitM (SequenceNumbered i a) (SequenceNumbered i a') m ()
overSequenceNumberedC f =
    awaitForever $
        \sn -> yield (_seqNumValue sn) .|
            mapOutput (MkSequenceNumbered (_seqNum sn)) (f (_seqNum sn))

synchronizeToSeqNum :: (Monad m, Integral i)
                    => i
                    -> ConduitM a (SequenceNumbered i a) m ()
synchronizeToSeqNum startSeq =
    evalStateC startSeq (awaitForever sendMkSync)
  where
    sendMkSync a = do
        nextSeq <- get
        modify (+ 1)
        yield (MkSequenceNumbered nextSeq a)

-- | Buffer incoming samples in a queue of the given size and output them in
-- order. The output is monotone increasing.
reorder :: (Ord a, Monad m) => Int -> Conduit a m a
reorder windowSize = go Set.empty Nothing
  where
    go queue minIndex = do
        mx <- await
        case mx of
            Nothing -> mapM_ yield queue
            Just x -> if maybe False (x <=) minIndex
                      then go queue minIndex
                      else let queue' = Set.insert x queue
                               mMinView = Set.minView queue'
                           in
                               if Set.size queue >= windowSize
                               then mapM_ (yield . fst) mMinView
                                   >> go (maybe queue' snd mMinView)
                                         (maybe minIndex
                                                (Just .
                                                     fst)
                                                mMinView)
                               else go queue' minIndex

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
