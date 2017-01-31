module Data.MediaBus.Frame
    ( Frame(..)
    , frame
    , MediaC(..)
    , type MediaSource
    , type MediaFilter
    , type MediaFilter'
    , type MediaSink
    , frameConverter
    , sampleBufferConverter
    , runMediaC
    , convertMediaC
    , connectMediaC
    ) where

import qualified Data.Vector.Storable            as V
import qualified Data.Vector.Storable.Mutable    as MV
import qualified Data.Vector.Generic.Mutable     as GMV
import           Conduit
import           Control.Lens
import           Control.Monad.Trans.Reader
import           Data.MediaBus.Sample
import           Data.MediaBus.Clock
import           Data.MediaBus.Internal.Monotone
import           Data.Function                   ( on )

newtype MediaC i o clock m =
      MkMediaC { mediaC :: Conduit i (ReaderT clock m) o }

type MediaSource i sample clock m = MediaC i (Frame sample clock) clock m

type MediaFilter sample sample' clock m = MediaC (Frame sample clock) (Frame sample' clock) clock m

type MediaFilter' sample clock m = MediaFilter sample sample clock m

type MediaSink sample clock o m = MediaC (Frame sample clock) o clock m

frameConverter :: Monad m
               => (clock -> Frame sample clock -> Frame sample' clock)
               -> MediaFilter sample sample' clock m
frameConverter f = MkMediaC (do
                                 f' <- lift (asks f)
                                 awaitForever (yield . f'))

sampleBufferConverter :: (Storable sample, Storable sample', Monad m)
                      => (SampleBuffer sample -> SampleBuffer sample')
                      -> MediaFilter sample sample' clock m
sampleBufferConverter f =
    frameConverter (const (over sampleBuffer f))

sampleConverter :: (Storable sample, Storable sample', Monad m)
                => (sample -> sample')
                -> MediaFilter sample sample' clock m
sampleConverter f = sampleBufferConverter (MkSampleBuffer .
                                               fst . unsafeMutateSamples go)
  where
    go vIn = do
        let len = GMV.length vIn
        vOut <- MV.new len
        GMV.fold
        V.unsafeFreeze vOut

convertMediaC :: Monad m
              => clock
              -> MediaSink sample clock (Frame sample' clock') m
              -> MediaSource (Frame sample clock) sample' clock' m
convertMediaC clk sink =
    MkMediaC (readerC (const (runMediaC clk sink)))

connectMediaC :: Monad m
              => MediaC i b clock m
              -> MediaC b o clock m
              -> MediaC i o clock m
connectMediaC (MkMediaC source) (MkMediaC sink) =
    MkMediaC (source .| sink)

runMediaC :: Monad m => r -> MediaC i o r m -> ConduitM i o m ()
runMediaC clk = runReaderC clk . mediaC

-- | A 'Frame' can be anything that has a start time and is exactly one time
-- unit long, it can respresent anything ranging from an audio buffer with 20ms
-- of audio to a single pulse coded audio sample, of course it could also be a
-- video frame or a chat message.
newtype Frame sample clock =
      MkFrame { _frame :: SynchronizedTo (ReferenceTime clock) (Event (Timestamp clock) (SampleBuffer sample))
              }

makeLenses ''Frame

instance IsMonotone (Timestamp clock) =>
         IsMonotone (Frame sample clock) where
    succeeds = succeeds `on` _frame

instance Storable sample =>
         HasSampleBuffer (Frame sample clock) where
    type SetSampleType (Frame sample clock) t = Frame t clock
    type GetSampleType (Frame sample clock) = sample
    sampleBuffer = frame . fromSynchronized . eventContent
