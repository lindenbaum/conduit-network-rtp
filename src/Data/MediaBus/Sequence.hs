module Data.MediaBus.Sequence
    ( SeqNum(..)
    , HasSeqNum(..)
    , fromSeqNum
    , reorder
    , reorderSeries
    , synchronizeToSeqNum
    , Discontinous(..)
    ) where

import qualified Data.Set                        as Set
import qualified Data.Map                        as Map
import           Test.QuickCheck                 ( Arbitrary(..) )
import           Conduit
import           Data.MediaBus.Internal.Monotone
import           Data.MediaBus.Internal.Series
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Word
import           Debug.Trace
import           Data.Default

class SetSeqNum t (GetSeqNum t) ~ t =>
      HasSeqNum t where
    type GetSeqNum t
    type SetSeqNum t s
    seqNum :: Lens t (SetSeqNum t s) (GetSeqNum t) s

instance (HasSeqNum a, HasSeqNum b, GetSeqNum a ~ GetSeqNum b) =>
         HasSeqNum (Series a b) where
    type GetSeqNum (Series a b) = GetSeqNum a
    type SetSeqNum (Series a b) t = Series (SetSeqNum a t) (SetSeqNum b t)
    seqNum f (Start a) = Start <$> seqNum f a
    seqNum f (Next b) = Next <$> seqNum f b

newtype SeqNum s = MkSeqNum { _fromSeqNum :: s }
    deriving (Num, Eq, Bounded, Enum, IsMonotone, Arbitrary, Default)

makeLenses ''SeqNum

instance HasSeqNum (SeqNum s) where
  type GetSeqNum (SeqNum s) = s
  type SetSeqNum (SeqNum s) s' = SeqNum s'
  seqNum = fromSeqNum

instance Show s =>
         Show (SeqNum s) where
    show (MkSeqNum s) = "SEQNUM: " ++ show s

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

synchronizeToSeqNum :: (HasSeqNum a, Monad m, Integral i)
                    => i
                    -> Conduit a m (SetSeqNum a i)
synchronizeToSeqNum startSeq =
    evalStateC startSeq (awaitForever yieldSeq)
  where
    yieldSeq a = do
        nextSeq <- get
        modify (+ 1)
        yield (a & seqNum .~ nextSeq)

-- | Buffer incoming samples in a queue of the given size and output them in
-- order. The output is monotone increasing.
reorder :: (Ord a, Monad m) => Int -> Conduit a m a
reorder windowSize = go Set.empty Nothing (0 :: SeqNum Word64)
  where
    go queue minIndex counter = do
        mx <- await
        case mx of
            Nothing -> mapM_ (yield . fst) queue
            Just x -> if maybe False (x <=) minIndex
                      then go queue minIndex (counter + 1)
                      else let queue' = Set.insert (x, counter) queue
                               mMinView = Set.minView queue'
                           in
                               if Set.size queue >= windowSize
                               then mapM_ (yield . fst . fst) mMinView
                                   >> go (maybe queue' snd mMinView)
                                         (maybe minIndex
                                                (Just .
                                                     fst .
                                                         fst)
                                                mMinView)
                                         (counter + 1)
                               else go queue' minIndex (counter + 1)

-- | Buffer incoming samples in a queue of the given size and output them in
-- order. The output is monotone increasing.
reorderSeries :: (Ord a, Monad m, Show b)
              => (b -> a)
              -> Int
              -> Conduit (Series a b) m (Series a b)
reorderSeries bToA windowSize = do
    traceM "BEGIN reordering"
    go Map.empty Nothing maxDrops
  where
    maxDrops = windowSize
    go queue minIndex dropsAllowed = do
        mx <- await
        case mx of
            Nothing ->
                flushQueue
            Just s@(Start a) -> do
                flushQueue
                yield s
                go Map.empty (Just a) maxDrops
            Just (Next x) -> let a = bToA x
                             in
                                 if Just a < minIndex
                                 then if dropsAllowed == 0
                                      then do
                                          traceM ">>>>>>>>>>>>>TOO MANY CONSECUTIVE DROPS, flushing and restarting"
                                          flushQueue
                                          go (Map.singleton a x)
                                             (Just a)
                                             maxDrops
                                      else do
                                          traceM (">>>>>>>>>>>>>DROPPING " ++
                                                      show x)
                                          go queue minIndex (dropsAllowed - 1)
                                 else let queue' = Map.insert a x queue
                                          mMinView = Map.minViewWithKey queue'
                                      in
                                          if Map.size queue >= windowSize
                                          then do
                                              mapM_ (yieldNext . snd . fst)
                                                    mMinView
                                              go (maybe queue' snd mMinView)
                                                 (maybe minIndex
                                                        (Just . fst . fst)
                                                        mMinView)
                                                 maxDrops
                                          else go queue' minIndex dropsAllowed
      where
        flushQueue = mapM_ yieldNext (snd <$> Map.toAscList queue)
        yieldNext = yield . Next

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
