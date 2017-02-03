{-# LANGUAGE UndecidableInstances #-}

module Data.MediaBus.Frame
    ( Frame(..)
    , type FrameBuffer
    , frame
    , FrameC(..)
    , type FrameSource
    , frameSource
    , frameSourceM
    , type FrameFilter
    , frameFilter
    , frameFilterM
    , type FrameBufferFilter
    , frameBufferFilter
    , frameBufferFilterM
    , sampleFilter
    , sampleFilterM
    , type FrameSink
    , frameSink
    , frameSinkM
    , runFrameC
    , connectFrameC
    , Transcoder(..)
    ) where

import           Conduit
import           Control.Monad
import           Control.Lens
import           Data.MediaBus.Sample
import           Data.MediaBus.Clock
import           Data.MediaBus.Internal.Monotone
import           Data.Function                    ( on )
import           Data.Kind

newtype FrameC i o m = MkFrameC { runFrameC :: ConduitM i o m () }

type FrameSource i sample clock m = FrameC i (Frame sample clock) m

frameSource :: Monad m
            => (i -> Frame sample clock)
            -> FrameSource i sample clock m
frameSource f = MkFrameC (awaitForever (yield . f))

frameSourceM :: Monad m
             => (i -> m (Frame sample clock))
             -> FrameSource i sample clock m
frameSourceM f = MkFrameC (awaitForever (lift . f >=> yield))

type FrameFilter sample sample' clock clock' m = FrameC (Frame sample clock) (Frame sample' clock') m

frameFilter :: Monad m
            => (Frame sample clock -> Frame sample' clock')
            -> FrameFilter sample sample' clock clock' m
frameFilter = frameSource

frameFilterM :: Monad m
             => (Frame sample clock -> m (Frame sample' clock'))
             -> FrameC (Frame sample clock) (Frame sample' clock') m
frameFilterM = frameSourceM

type FrameBufferFilter sample sample' clock clock' m = FrameC (FrameBuffer sample clock) (FrameBuffer sample' clock') m

frameBufferFilter :: (Storable sample, Storable sample', Monad m)
                  => (SampleBuffer sample -> SampleBuffer sample')
                  -> FrameBufferFilter sample sample' clock clock m
frameBufferFilter f = frameFilter (over sampleBuffer f)

frameBufferFilterM :: (Storable sample, Storable sample', Monad m)
                   => (SampleBuffer sample -> m (SampleBuffer sample'))
                   -> FrameBufferFilter sample sample' clock clock m
frameBufferFilterM f = frameFilterM $
    \frm -> do
        let buffer = frm ^. sampleBuffer
        buffer' <- f buffer
        return (frm & sampleBuffer .~ buffer')

sampleFilter :: (Storable sample, Storable sample', Monad m)
             => (sample -> sample')
             -> FrameBufferFilter sample sample' clock clock m
sampleFilter f = frameBufferFilter (over (sampleVector . each) f)

sampleFilterM :: (Storable sample, Storable sample', Monad m)
              => (sample -> m sample')
              -> FrameBufferFilter sample sample' clock clock m
sampleFilterM f = frameBufferFilterM $ mapMOf (sampleVector . each) f

type FrameSink sample clock o m = FrameC (Frame sample clock) o m

frameSink :: Monad m => (Frame sample clock -> o) -> FrameSink sample clock o m
frameSink f = MkFrameC (awaitForever (yield . f))

frameSinkM :: Monad m
           => (Frame sample clock -> m o)
           -> FrameSink sample clock o m
frameSinkM f = MkFrameC (awaitForever (lift . f >=> yield))

connectFrameC :: Monad m => FrameC i b m -> FrameC b o m -> FrameC i o m
connectFrameC (MkFrameC source) (MkFrameC sink) =
    MkFrameC (source .| sink)

-- | A 'Frame' can be anything that has a start time and is exactly one time
-- unit long, it can respresent anything ranging from an audio buffer with 20ms
-- of audio to a single pulse coded audio sample, of course it could also be a
-- video frame or a chat message.
newtype Frame content (clock :: Type) =
      MkFrame { _frame :: SynchronizedTo (Reference (Ticks clock)) (Ticks clock) content
              }

type FrameBuffer sample clock = Frame (SampleBuffer sample) clock

makeLenses ''Frame

instance IsMonotone (Ticks clock) =>
         IsMonotone (Frame s (clock :: Type)) where
    succeeds = succeeds `on` view (frame . ticks')

instance HasSampleBuffer content =>
         HasSampleBuffer (Frame content clock) where
    type SetSampleType (Frame content clock) t = Frame (SetSampleType content t) clock
    type GetSampleType (Frame content clock) = GetSampleType content
    sampleBuffer = frame . fromSynchronized . eventContent . sampleBuffer

class Transcoder from to clock where
    transcode :: Monad m => FrameBufferFilter from to clock clock m
