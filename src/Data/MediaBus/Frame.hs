module Data.MediaBus.Frame
    ( Frame(..)
    , frameStart
    , frameContent
    ) where

import           Control.Lens
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
-- | A 'Frame' can be anything that has a start time and is exactly one time
-- unit long, it can respresent anything ranging from an audio buffer with 20ms
-- of audio to a single pulse coded audio sample, of course it could also be a
-- video frame or a chat message.
data Frame t b = MkFrame { _frameStart :: t
                         , _frameContent  :: b
                         }

makeLenses ''Frame

-- | A class of types, that describe some media /format/.
instance HasSampleFormat b =>
         HasSampleFormat (Frame t b) where
    type GetSampleFormat (Frame t b) = GetSampleFormat b
    type SetSampleFormat (Frame t b) b' = Frame t (SetSampleFormat b b')
    type ToSampleType (Frame t b) = ToSampleType b
    sampleFormat = frameContent . sampleFormat

instance HasTimestamp (Frame t b) where
    type SetTimestamp (Frame t b) t' = (Frame t' b)
    type GetTimestamp (Frame t b) = t
    timestamp = frameStart

instance HasSampleBuffer b =>
         HasSampleBuffer (Frame t b) where
    type SetSampleType (Frame t b) b' = Frame t (SetSampleType b b')
    type GetSampleType (Frame t b) = GetSampleType b
    sampleBuffer = frameContent . sampleBuffer

instance (Show t, Show b) =>
         Show (Frame t b) where
    show (MkFrame t b) = "<@ " ++
        show t ++ " - " ++ show b ++ " @>"

instance Eq t =>
         Eq (Frame t b) where
    (==) = (==) `on` _frameStart

instance Ord t =>
         Ord (Frame t b) where
    compare = compare `on` _frameStart

instance Functor (Frame t) where
    fmap f (MkFrame t x) = MkFrame t (f x)
