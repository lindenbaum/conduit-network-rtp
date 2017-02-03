{-# LANGUAGE UndecidableInstances #-}

module Data.MediaBus.Sample
    ( SampleBuffer(..)
    , sampleBufferToList
    , sampleBufferFromList
    , sampleVector
    , createSampleBufferFrom
    , HasSampleBuffer(..)
    , type GetSampleBuffer
    , mutateSamples
    , unsafeMutateSamples
    ) where

import           Control.Lens
import           Data.Vector.Storable         as SV
import           Data.Vector.Storable.Mutable as M ( MVector(..) )
import           Control.Monad.ST             ( ST, runST )
import           GHC.Exts                     ( IsList(..) )

-- | A sample is a discrete value of a continuous signal, periodically sampled
-- at the sampling frequency. This is a full buffer of those things.
newtype SampleBuffer sampleType =
      MkSampleBuffer { _sampleVector :: SV.Vector sampleType }
    deriving (Show, Eq, Monoid)

makeLenses ''SampleBuffer

instance Storable s =>
         IsList (SampleBuffer s) where
    type Item (SampleBuffer s) = s
    fromList = sampleBufferFromList
    toList = sampleBufferToList

sampleBufferToList :: Storable s => SampleBuffer s -> [s]
sampleBufferToList = SV.toList . _sampleVector

sampleBufferFromList :: Storable s => [s] -> SampleBuffer s
sampleBufferFromList = MkSampleBuffer . SV.fromList

createSampleBufferFrom :: (Storable sample')
                       => (forall s.
                           SV.Vector sample -> ST s (MVector s sample'))
                       -> SampleBuffer sample
                       -> SampleBuffer sample'
createSampleBufferFrom f =
    over sampleVector (\v -> SV.create (f v))

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

type GetSampleBuffer s = SampleBuffer (GetSampleType s)

instance Storable a =>
         HasSampleBuffer (SampleBuffer a) where
    type GetSampleType (SampleBuffer a) = a
    type SetSampleType (SampleBuffer a) t = SampleBuffer t
    sampleCount = SV.length . _sampleVector
    eachSample = sampleVector . each
    sampleBuffer = lens id (flip const)

mutateSamples :: Storable a
              => (forall s. M.MVector s a -> ST s ())
              -> SampleBuffer a
              -> SampleBuffer a
mutateSamples f (MkSampleBuffer v) =
    MkSampleBuffer (SV.modify f v)

-- | Unsafe because results can be returned, which might contain the /thawn/ vector.
unsafeMutateSamples :: Storable a
                    => (forall s. M.MVector s a -> ST s r)
                    -> SampleBuffer a
                    -> (r, SampleBuffer a)
unsafeMutateSamples f (MkSampleBuffer v) =
    runST $ do
        mv <- unsafeThaw v
        r <- f mv
        v' <- unsafeFreeze mv
        return (r, MkSampleBuffer v')
