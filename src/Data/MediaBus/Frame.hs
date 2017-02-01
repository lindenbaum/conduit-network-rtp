module Data.MediaBus.Frame
    ( Frame(..)
    , frame
    , MediaC(..)
    , type MediaSource
    , type MediaFilter
    , type MediaSink
    , frameConverter
    , sampleBufferConverter
    , sampleConverter
    , statefulConverter
    , runMediaC
    , connectMediaC
    ) where

import qualified Data.Vector.Storable             as V
import qualified Data.Vector.Storable.Mutable     as MV
import qualified Data.Vector.Generic.Mutable      as GMV
import           Conduit
import           Control.Lens
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           Data.MediaBus.Sample
import           Data.MediaBus.Clock
import           Data.MediaBus.Internal.Monotone
import           Data.Function                    ( on )
import Conduit

newtype MediaC i o m = MkMediaC { runMediaC :: Conduit i m o }

type MediaSource i sample clock m = MediaC i (Frame sample clock) m

type MediaFilter sample sample' clock clock' m = MediaC (Frame sample clock) (Frame sample' clock') m

type MediaSink sample clock o m = MediaC (Frame sample clock) o m

frameConverter :: Monad m
               => (Frame sample clock -> Frame sample' clock')
               -> MediaC (Frame sample clock) (Frame sample' clock') m
frameConverter f = MkMediaC (awaitForever (yield . f))

sampleBufferConverter :: (Storable sample, Storable sample', Monad m)
                      => (SampleBuffer sample -> SampleBuffer sample')
                      -> MediaC (Frame sample clock) (Frame sample' clock) m
sampleBufferConverter f =
    frameConverter (over sampleBuffer f)

sampleConverter :: (Storable sample, Storable sample', Monad m)
                => (sample -> sample')
                -> MediaC (Frame sample clock) (Frame sample' clock) m
sampleConverter f = sampleBufferConverter (over (sampleVector . each) f)

statefulConverter :: Monad m
                  => (Frame sample (clock :: k) -> State st (Frame sample' (clock' :: k)))
                  -> st
                  -> MediaC (Frame sample clock) (Frame sample' clock') m
statefulConverter f stIn =
    MkMediaC (evalStateC stIn (awaitForever go))
  where
    go frm = do
        st <- lift get
        let (frm', st') = runState (f frm) st
        lift (put st')
        yield frm'

connectMediaC :: Monad m => MediaC i b m -> MediaC b o m -> MediaC i o m
connectMediaC (MkMediaC source) (MkMediaC sink) =
    MkMediaC (source .| sink)

-- | A 'Frame' can be anything that has a start time and is exactly one time
-- unit long, it can respresent anything ranging from an audio buffer with 20ms
-- of audio to a single pulse coded audio sample, of course it could also be a
-- video frame or a chat message.
newtype Frame sample clock =
      MkFrame { _frame :: SynchronizedTo (ReferenceTime clock) (Timestamp clock) (SampleBuffer sample)
              }

makeLenses ''Frame

instance Storable sample =>
         HasSampleBuffer (Frame sample clock) where
    type SetSampleType (Frame sample clock) t = Frame t clock
    type GetSampleType (Frame sample clock) = sample
    sampleBuffer = frame . fromSynchronized . eventContent
