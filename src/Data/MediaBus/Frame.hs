module Data.MediaBus.Frame
    ( type Frame
    , MediaC(..)
    , mediaCClock
    , mediaC
    , type MediaSource
    , type MediaFilter
    , type MediaFilter'
    , type MediaSink
    ) where

import           Conduit
import           Control.Lens
import           Data.MediaBus.Sample
import           Data.MediaBus.Clock

data MediaC i o clock m =
      MkMediaC { _mediaCClock :: clock
               , _mediaC      :: Conduit i m o
               }

makeLenses ''MediaC

type MediaSource i sample clock m = MediaC i (Frame sample clock) clock m

type MediaFilter sample sample' clock m = MediaC (Frame sample clock) (Frame sample' clock) clock m

type MediaFilter' sample clock m = MediaFilter sample sample clock m

type MediaSink sample clock o m = MediaC (Frame sample clock) o clock m

-- | A 'Frame' can be anything that has a start time and is exactly one time
-- unit long, it can respresent anything ranging from an audio buffer with 20ms
-- of audio to a single pulse coded audio sample, of course it could also be a
-- video frame or a chat message.
type Frame sample clock = SynchronizedTo (ReferenceTime clock) (Event (Timestamp clock) (SampleBuffer sample))
