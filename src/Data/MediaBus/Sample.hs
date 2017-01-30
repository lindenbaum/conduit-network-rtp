{-# LANGUAGE UndecidableInstances #-}
module Data.MediaBus.Sample
    ( HasSampleBuffer(..)
    , SampleBuffer(..)
    , sampleVector
    , mutateSamples
    , unsafeMutateSamples
    , module X
    ) where

import           Control.Lens
import           Data.Vector.Storable        as X ( Storable, fromList, toList )
import           Data.Vector.Storable        as SV ( Vector, modify
                                                   , unsafeFreeze, unsafeThaw )
import           Data.Vector.Generic.Mutable as X ( MVector(..) )
import           Control.Monad.ST            as X ( ST, runST )

-- | A type class that abstracts over traversable/foldable media data storage
newtype SampleBuffer sampleType =
      MkSampleBuffer { _sampleVector :: SV.Vector sampleType }
  deriving (Show, Eq)

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
