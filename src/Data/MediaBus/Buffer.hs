module Data.MediaBus.Buffer
    ( HasSampleBuffer(..)
    , SampleBuffer(..)
    , sampleVector
    , mutateSamples
    , module X
    ) where

import           Control.Lens
import           Data.Vector.Storable        as X ( Storable )
import           Data.Vector.Storable        as SV ( Vector, modify )
import           Data.Vector.Generic.Mutable as X ( MVector(..) )
import           Control.Monad.ST            as X ( ST )

-- | A type class that abstracts over traversable/foldable media data storage
newtype SampleBuffer a = MkSampleBuffer { _sampleVector :: SV.Vector a }
    deriving Show

makeLenses ''SampleBuffer

-- | A type class for media formats, like encodings, sample rate, etc...
class (Storable (GetSampleType s), SetSampleType s (GetSampleType s) ~ s) =>
      HasSampleBuffer s where
    type SetSampleType s t
    type GetSampleType s
    eachSample :: Traversal' s (GetSampleType s)
    eachSample = sampleBuffer . sampleVector . each
    sampleBuffer :: Storable t
                 => Lens s (SetSampleType s t) (SampleBuffer (GetSampleType s)) (SampleBuffer t)

instance Storable a =>
         HasSampleBuffer (SampleBuffer a) where
    type GetSampleType (SampleBuffer a) = a
    type SetSampleType (SampleBuffer a) t = SampleBuffer t
    eachSample = sampleVector . each
    sampleBuffer = lens id (flip const)

mutateSamples :: Storable a
              => SampleBuffer a
              -> (forall s v. X.MVector v a => v s a -> ST s ())
              -> SampleBuffer a
mutateSamples (MkSampleBuffer v) f =
    MkSampleBuffer (SV.modify f v)
