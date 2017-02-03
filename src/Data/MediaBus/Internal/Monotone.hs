module Data.MediaBus.Internal.Monotone
    ( Reference(..)
    , fromReference
    , IsMonotone(..)
    ) where

import           Data.Word
import           Data.Int
import           Test.QuickCheck
import           Control.Lens

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

-- | An offset, e.g. like the first rndom RTP timestamp to which the following
-- timestamps relate.
newtype Reference s = MkReference { _fromReference :: s }
    deriving (Bounded, Integral, Num, Enum, Real, Ord, Eq, Arbitrary, Functor, IsMonotone)

makeLenses ''Reference

instance Show s =>
         Show (Reference s) where
    show (MkReference x) = "#" ++ show x
