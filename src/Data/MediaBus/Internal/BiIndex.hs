-- | Combine two bounded indices into one, backed by a common index large enoug,
-- such that after every 'Num' or 'Integral' operation on the 'BiIndex' the
-- 'major' and 'minor' indices update consistently.
--
-- More formally, they are distributive:
--   @major (b & baseIndex +~ x) == major b + x `modRange` majorRange b@
--   @minor (over baseIndex (+ x) b)  == minor b + x `modRange` minorRange b@
--
-- Both indices are bounded, /wrap around/ style indices, which are consistent.
--
-- E.g. if the major range were @(0, 255)@ and the minor range was @(0,10)@, and
-- the major value was currently @255@ and the minor value was @4@, than adding
-- @1@ to the 'BiIndex' would make the next 'major' value to be @0@ while
-- 'minor' would be @5@.
module Data.MediaBus.Internal.BiIndex
    ( BiIndex()
    , baseIndex
    , mkBiIndex
    , major
    , majorRange
    , majorRangeSize
    , majorOffset
    , minor
    , minorRange
    , minorRangeSize
    , minorOffset
    , modRange
    ) where

import           Test.QuickCheck
import           Control.Lens
import           Text.Printf

-- | Wrapper around to integer ranges.
data BiIndex a b = MkBiIndex { _baseIndex :: Integer
                             , upperBound :: Integer
                             , majorRange :: (a, a)
                             , minorRange :: (b, b)
                             }

instance (Show a, Integral a, Show b, Integral b) =>
         Show (BiIndex a b) where
    show b = printf "«☝%s of [%s .. %s] - ☟ %s of [%s .. %s] ← %d of [0 .. %d] »"
                    (show (major b))
                    (show (fst (majorRange b)))
                    (show (snd (majorRange b)))
                    (show (minor b))
                    (show (fst (minorRange b)))
                    (show (snd (minorRange b)))
                    (view baseIndex b)
                    (upperBound b)

-- | Create a 'BiIndex' adapted to be mapped to two distinct ranges, operations
-- on the 'BiIndex' always respect both ranges, without causing /glitches/.
mkBiIndex :: (Integral a, Integral b) => (a, a) -> (b, b) -> BiIndex a b
mkBiIndex majorRange minorRange =
    MkBiIndex { _baseIndex = 0, upperBound, majorRange, minorRange }
  where
    upperBound = let g = gcd majSize minSize
                     majSize = rangeSize majorRange
                     minSize = rangeSize minorRange
                 in
                     g * (majSize `div` g) *
                         (minSize `div` g)

-- | A lens to get/set the BiIndex
baseIndex :: Lens' (BiIndex a b) Integer
baseIndex f b@MkBiIndex{_baseIndex,upperBound} =
    store <$> f _baseIndex
  where
    store _baseIndex' = b { _baseIndex = _baseIndex' `mod` upperBound }

-- | A getter for the major index.
major :: Integral a => BiIndex a b -> a
major b = _baseIndex b `modRange` majorRange b

-- | Return the minor range size as an 'Integer' (for precision)
majorRangeSize :: Integral a => BiIndex a b -> Integer
majorRangeSize = rangeSize . majorRange

-- | Return the major range size as an 'Integer' (for precision)
majorOffset :: Integral a => BiIndex a b -> Integer
majorOffset MkBiIndex{majorRange} =
    toInteger (fst majorRange)

-- | A getter for the minor index.
minor :: Integral b => BiIndex a b -> b
minor b = fromIntegral ((_baseIndex b `rem` minorRangeSize b) + minorOffset b)

-- | Return the minor range size as an 'Integer' (for precision)
minorRangeSize :: Integral b => BiIndex a b -> Integer
minorRangeSize = rangeSize . minorRange

-- | Return the minor range size as an 'Integer' (for precision)
minorOffset :: Integral b => BiIndex a b -> Integer
minorOffset MkBiIndex{minorRange} =
    toInteger (fst minorRange)

-- | Calculate the range size as an 'Integer' (for precision) from a range
-- @(a,a)@.
rangeSize :: Integral a => (a, a) -> Integer
rangeSize (rangeMin, rangeMax) =
    1 + abs (toInteger rangeMax - toInteger rangeMin)

-- | Map an integer into a range with module arithmetic
modRange :: Integral a => Integer -> (a, a) -> a
modRange i r@(offset, _) =
    fromIntegral ((i `mod` rangeSize r) + toInteger offset)

instance (Integral a, Arbitrary a, Integral b, Arbitrary b) =>
         Arbitrary (BiIndex a b) where
    arbitrary = do
        m1 <- arbitrary
        m2 <- arbitrary
        n1 <- arbitrary
        n2 <- arbitrary
        x <- arbitrary
        return (set baseIndex
                    x
                    (mkBiIndex (min m1 m2, max m1 m2) (min n1 n2, max n1 n2)))
