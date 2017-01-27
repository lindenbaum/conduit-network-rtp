module Data.MediaBus.Sample
    ( Sample(..)
    , sampleTimestamp
    , sampleContent
    ) where

import           Control.Lens
import           Data.MediaBus.Basics
import           Data.MediaBus.Buffer
import           Data.MediaBus.Clock
import           Data.Function        ( on )

-- | Indication of a state change in a stream
-- data Event a b = Init a
--                | Handle b
--                | Terminate
--     deriving (Show)
-- instance Functor (Event a) where
--     fmap f (Handle x) = Handle (f x)
--     fmap _f (Init x) = Init x
--     fmap _f Terminate = Terminate
-- | A 'Sample' can be anything that has a start time and is exactly one time
-- unit long, it can respresent anything ranging from an audio buffer with 20ms
-- of audio to a single pulse coded audio sample, of course it could also be a
-- video frame or a chat message.
data Sample t b = MkSample { _sampleTimestamp :: t
                           , _sampleContent   :: b
                           }

makeLenses ''Sample

instance HasFormat b =>
         HasFormat (Sample t b) where
    type GetFormat (Sample t b) = GetFormat b
    type SetFormat (Sample t b) b' = Sample t (SetFormat b b')
    format = sampleContent . format

instance HasTimestamp (Sample t b) where
    type SetTimestamp (Sample t b) t' = (Sample t' b)
    type GetTimestamp (Sample t b) = t
    timestamp = sampleTimestamp

instance HasSampleBuffer b => HasSampleBuffer (Sample t b) where
    type SetBuffer (Sample t b) b' = (Sample t b')
    type GetSampleType (Sample t b) = GetSampleType b
    sampleContent = sampleContent . buffer

instance (Show t, Show b) =>
         Show (Sample t b) where
    show (MkSample t b) = "<@ " ++
        show t ++ " - " ++ show b ++ " @>"

instance Eq t =>
         Eq (Sample t b) where
    (==) = (==) `on` _sampleTimestamp

instance Ord t =>
         Ord (Sample t b) where
    compare = compare `on` _sampleTimestamp

instance Functor (Sample t) where
    fmap f (MkSample t x) = MkSample t (f x)

instance HasSampleBuffer b =>
         HasSampleBuffer (Sample t b) where
    type SetSampleType (Sample t b) b' = Sample t (SetSampleType b b')
    type GetSampleType (Sample t b) = b
    eachSample = sampleContent . eachSample
    sampleBuffer = sampleContent . sampleBuffer
