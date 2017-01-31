{-# LANGUAGE UndecidableInstances #-}

module Data.MediaBus.Sample
    ( HasSampleBuffer(..)
    , SampleBuffer(..)
    , sampleVector
    , mutateSamples
    , unsafeMutateSamples
    , module X
    ) where

import           Data.MediaBus.Clock
import           Control.Lens
import           Data.Word
import           Data.Vector.Storable        as X ( Storable, fromList, toList )
import           Data.Vector.Storable        as SV ( Vector, length, modify
                                                   , unsafeFreeze, unsafeThaw )
import           Data.Vector.Generic.Mutable as X ( MVector(..) )
import           Control.Monad.ST            as X ( ST, runST )
import           GHC.TypeLits

class KnownNat (GetSamplingRate a) =>
      HasSamplingRate a where
    type GetSamplingRate a :: Nat
    duration :: a -> Ticks (GetSamplingRate a)

-- | A wrapper for the number of ticks at a certain rate.
newtype Ticks (samplingRate :: Nat) = MkTicks { fromTicks :: Word64 }
    deriving (Show, Eq, Ord, Num, Integral, Enum, X.Storable, Real)

-- | A sample is a discrete value of a continuous signal, periodically sampled
-- at the sampling frequency. This is a full buffer of those things.
newtype SampleBuffer (samplingRate :: Nat) sampleType =
      MkSampleBuffer { _sampleVector :: SV.Vector sampleType }
    deriving (Show, Eq)

instance (KnownNat samplingRate, HasSamplingRate a) =>
         HasSamplingRate (SampleBuffer samplingRate a) where
    type GetSamplingRate (SampleBuffer samplingRate a) = samplingRate
    duration (MkSampleBuffer v) =
        MkTicks (SV.length v)

newtype AdaptSamplingRate (samplingRate :: Nat) a =
      MkAdaptSamplingRate { fromAdaptSamplingRate :: a }
    deriving (Show, Eq, Ord, Num, Integral, Enum, X.Storable, Real)

instance (KnownNat samplingRate, HasSamplingRate a) =>
         HasSamplingRate (AdaptSamplingRate samplingRate a) where
    type GetSamplingRate (AdaptSamplingRate samplingRate a) = samplingRate
    duration (MkAdaptSamplingRate a) =
        let ticksIn = duration a
            rateIn = natVal ticksIn
            rateOut = natVal ticksOut
        in
            MkTicks ((fromTicks ticksIn * rateOut) `div` rateIn)

makeLenses ''SampleBuffer

-- | A type class for media formats, like encodings, sample rate, etc...
class (Storable (GetSampleType s), SetSampleType s (GetSampleType s) ~ s) =>
      HasSampleBuffer s where
    type SetSampleType s t
    type GetSampleType s
    sampleCount :: s -> Int
    eachSample :: Traversal' s (GetSampleType s)
    eachSample = sampleBuffer . sampleVector . each
    sampleBuffer :: Storable t
                 => Lens s (SetSampleType s t) (SampleBuffer (GetSampleType s)) (SampleBuffer t)

instance Storable a =>
         HasSampleBuffer (SampleBuffer a) where
    type GetSampleType (SampleBuffer a) = a
    type SetSampleType (SampleBuffer a) t = SampleBuffer t
    sampleCount = SV.length . view sampleVector
    eachSample = sampleVector . each
    sampleBuffer = lens id (flip const)

mutateSamples :: Storable a
              => (forall s v. X.MVector v a => v s a -> ST s ())
              -> SampleBuffer a
              -> SampleBuffer a
mutateSamples f (MkSampleBuffer v) =
    MkSampleBuffer (SV.modify f v)

-- | Unsafe because results can be returned, which might contain the /thawn/ vector.
unsafeMutateSamples :: Storable a
                    => (forall s v. X.MVector v a => v s a -> ST s r)
                    -> SampleBuffer a
                    -> (r, SampleBuffer a)
unsafeMutateSamples f (MkSampleBuffer v) =
    runST $ do
        mv <- unsafeThaw v
        r <- f mv
        v' <- unsafeFreeze mv
        return (r, MkSampleBuffer v')
