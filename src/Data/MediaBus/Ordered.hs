module Data.MediaBus.Ordered
    ( IsMonotone(..)
    , Monotone(..)
    , reorder
    , Discontinous(..)
    ) where

import qualified Data.Set             as Set
import           Data.Word
import           Data.Int
import           Test.QuickCheck      ( Arbitrary(..) )
import           Conduit
import           Data.MediaBus.Basics

-- -----------------------------------------------------------------------------
-- * Media Data Ordering
-- -----------------------------------------------------------------------------x
-- | Class of numbers that are monotone increasing (or decreasing) and have a
-- relative order, that is not necessarily transitive.
--
-- For example, for a series of 'Word8' values: @0 64 128 192 0 64 128 ...@
-- could be interpreted as a monotone series of consecutive increasing values,
-- that wrap around after 255. But note that the 'Ord' instance is not
-- sufficient to express that @0@ is __after__ @192@, since @0 < 192@.
class IsMonotone a where
    succeeds :: a -> a -> Bool
    default succeeds :: (Bounded a, Integral a) => a -> a -> Bool
    x `succeeds` y = (x - y) < ((maxBound - minBound) `div` 2)

instance IsMonotone Word8

instance IsMonotone Word16

instance IsMonotone Word32

instance IsMonotone Word64

instance IsMonotone Int8

instance IsMonotone Int16

instance IsMonotone Int32

instance IsMonotone Int64

instance IsMonotone Int

newtype Monotone a = MkMonotone { fromMonotone :: a }
    deriving (Show, Num, Eq, Integral, Real, Bounded, Enum, IsMonotone, Arbitrary)

instance (Eq a, IsMonotone a) =>
         Ord (Monotone a) where
    compare x y = if x == y then EQ else if x `succeeds` y then GT else LT

-- | Buffer incoming samples in a queue of the given size and output them in
-- order. The output is monotone increasing.
reorder :: (Eq t, IsMonotone t, Monad m) => Int -> (a -> t) -> Conduit a m a
reorder windowSize indexOf =
    go Set.empty Nothing
  where
    go queue minIndex = do
        mx <- await
        case mx of
            Nothing -> mapM_ (yield . _sampleData) queue
            Just x -> let xt = indexOf x
                      in
                          if maybe False (not . succeeds xt) minIndex
                          then go queue minIndex
                          else let queue' = Set.insert (MkSample (MkMonotone xt)
                                                                 x)
                                                       queue
                                   mMinView = Set.minView queue'
                               in
                                   if Set.size queue >= windowSize
                                   then mapM_ (yield . _sampleData . fst)
                                              mMinView
                                       >> go (maybe queue' snd mMinView)
                                             (maybe minIndex
                                                    (Just .
                                                         fromMonotone .
                                                             _presentationTime .
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
