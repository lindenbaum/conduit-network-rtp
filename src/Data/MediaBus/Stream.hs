module Data.MediaBus.Stream
    ( type SourceId'
    , type SeqNum'
    , type Ticks'
    , type Timing'
    , FrameCtx(..)
    , type FrameCtx'
    , frameCtxSourceId
    , frameCtxSeqNumRef
    , frameCtxTimestampRef
    , Frame(..)
    , type Frame'
    , frameSeqNum
    , frameTimestamp
    , frameValue
    , Stream(..)
    , Stream'
    , stream
    , yieldStreamish
    , foldStreamC
    , mapStreamC
    , Transcoder(..)
    , overStreamC
    , overFramesC
    , frameResamplerM
    , type StreamSink
    , type StreamSink'
    , foldStream
    , foldStreamM
    , concatStreamContents
    ) where

import           Conduit
import           Control.Monad
import           Control.Lens
import           Data.MediaBus.SourceId
import           Data.MediaBus.Sequence
import           Data.MediaBus.Clock
import           Data.MediaBus.Internal.Series
import           Control.Monad.Writer.Strict   ( tell )
import           Data.Maybe
import           Data.Word
import           Test.QuickCheck
import           Data.Kind
import           Data.Default
import           Text.Printf

-- data RawFrame i s t v = MkRawFrame { _rawFrameSourceId  :: i
--                                    , _rawFrameSeqNum    :: s
--                                    , _rawFrameTimestamp :: t
--                                    , _rawFrameValue     :: v
--                                    }
-- makeLenses ''RawFrame
-- instance (Show i, Show s, Show t, Show v) =>
--          Show (RawFrame i s t v) where
--     show (MkRawFrame i s t v) =
--         "RAW ** " ++
--             show i ++
--                 " ** " ++
--                     show s ++
--                         " ** " ++
--                             show t ++
--                                 " ** " ++
--                                     show v
type SourceId' = SourceId Word32

type SeqNum' = SeqNum Word16

type Timing' r = Timing r Word32

type Ticks' r = Ticks (Timing' r)

data FrameCtx i s t = MkFrameCtx { _frameCtxSourceId     :: i
                                 , _frameCtxTimestampRef :: t
                                 , _frameCtxSeqNumRef    :: s
                                 }
    deriving (Eq, Ord)

type FrameCtx' r = FrameCtx SourceId' SeqNum' (Ticks' r)

makeLenses ''FrameCtx

instance HasTimestamp (FrameCtx i s t) where
    type GetTimestamp (FrameCtx i s t) = t
    type SetTimestamp (FrameCtx i s t) t' = (FrameCtx i s t')
    timestamp = frameCtxTimestampRef

instance (Arbitrary i, Arbitrary s, Arbitrary t) =>
         Arbitrary (FrameCtx i s t) where
    arbitrary = MkFrameCtx <$> arbitrary <*> arbitrary <*> arbitrary

instance (Default i, Default s, Default t) =>
         Default (FrameCtx i s t) where
    def = MkFrameCtx def def def

instance (Show i, Show s, Show t) =>
         Show (FrameCtx i s t) where
    show (MkFrameCtx sid tsr snr) =
        printf "FRAME-CTX: %15s | %15s | %15s" (show sid) (show snr) (show tsr)

-- | A 'Frame' can be anything that has a start time and is exactly one time
-- unit long, it can respresent anything ranging from an audio buffer with 20ms
-- of audio to a single pulse coded audio sample, of course it could also be a
-- video frame or a chat message.
data Frame s t c = MkFrame { _frameTimestamp :: t
                           , _frameSeqNum    :: s
                           , _frameValue     :: c
                           }
    deriving (Eq, Ord)

deriving instance Functor (Frame s t)

type Frame' r v = Frame SeqNum' (Ticks' r) v

makeLenses ''Frame

instance HasTimestamp (Frame s t c) where
    type GetTimestamp (Frame s t c) = t
    type SetTimestamp (Frame s t c) t' = Frame s t' c
    timestamp = frameTimestamp

instance HasDuration c =>
         HasDuration (Frame s t c) where
    getDuration = getDuration . _frameValue

instance (Arbitrary c, Arbitrary s, Arbitrary t) =>
         Arbitrary (Frame s t c) where
    arbitrary = MkFrame <$> arbitrary <*> arbitrary <*> arbitrary

instance (Show s, Show t, Show v) =>
         Show (Frame s t v) where
    show (MkFrame ts sn v) =
        printf "FRAME: %15s | %15s | %s" (show sn) (show ts) (show v)

newtype Stream i s t c = MkStream { _stream :: Streamish i s t c }
    deriving (Ord, Eq, Arbitrary)

type Streamish i s t c = Series (FrameCtx i s t) (Frame s t c)

type Stream' t c = Stream SourceId' SeqNum' (Ticks' t) c

yieldStreamish :: Monad m => Streamish i s t c -> Source m (Stream i s t c)
yieldStreamish = yield . MkStream

foldStreamC :: Monad m
            => (StartingFrom (FrameCtx i s t) -> Conduit (Frame s t c) m o)
            -> Conduit (Stream i s t c) m o
foldStreamC = mapInput _stream (Just . MkStream) . foldSeriesC

mapStreamC :: Monad m
           => Conduit (Streamish i s t c) m (Streamish i' s' t' c')
           -> Conduit (Stream i s t c) m (Stream i' s' t' c')
mapStreamC = mapInput _stream (Just . MkStream) . mapOutput MkStream

makeLenses ''Stream

instance HasTimestamp (Stream i s t c) where
    type GetTimestamp (Stream i s t c) = t
    type SetTimestamp (Stream i s t c) t' = Stream i s t' c
    timestamp = stream . timestamp

instance (Show i, Show s, Show t, Show c) =>
         Show (Stream i s t c) where
    show (MkStream s) = show s

class Transcoder from to s t where
    type TranscodingM from to (m :: Type -> Type) :: Constraint
    type TranscodingM from to m = Monad m
    transcode :: TranscodingM from to m
              => Conduit (Frame s t from) m (Frame s t to)

overStreamC :: Monad m
            => Conduit (Series (FrameCtx i s t) (Frame s t c)) m (Series (FrameCtx i' s' t') (Frame s' t' c'))
            -> Conduit (Stream i s t c) m (Stream i' s' t' c')
overStreamC = mapInput _stream (Just . MkStream) . mapOutput MkStream

overFramesC :: (Default i, Monad m)
            => (StartingFrom (FrameCtx i s t)
                -> Conduit (Frame s t from) m (Frame s t to))
            -> Conduit (Stream i s t from) m (Stream i s t to)
overFramesC f = overStreamC process
  where
    process = overSeriesC' toInitialCtx f
      where
        toInitialCtx (MkFrame s t _) =
            MkFrameCtx def s t

frameResamplerM :: forall i s t t' c c' m.
                Monad m
                => (t -> t')
                -> (c -> m c')
                -> ConduitM (Stream i s t c) (Stream i s t' c') m ()
frameResamplerM fTicks fContent =
    awaitForever (doContent . doTicks >=> yield)
  where
    doContent = lift .
        mapMOf (stream .
                    _Next .
                        frameValue)
               fContent
    doTicks = over stream
                   (over (_Start . frameCtxTimestampRef) fTicks .
                        over (_Next . frameTimestamp) fTicks)

type StreamSink i s t c m r = Sink (Stream i s t c) m r

type StreamSink' t c m r = StreamSink SourceId' SeqNum' (Ticks' t) c m r

foldStream :: (Monoid o, Monad m)
           => (Stream i s t c -> o)
           -> StreamSink i s t c m o
foldStream f = execWriterC $
    awaitForever $
        tell .
            f

foldStreamM :: (Monoid o, Monad m)
            => (Stream i s t c -> m o)
            -> StreamSink i s t c m o
foldStreamM f = execWriterC $
    awaitForever (lift . lift . f >=> tell)

concatStreamContents :: (Monoid c, Monad m) => StreamSink i s t c m c
concatStreamContents = foldStream (fromMaybe mempty .
                                       (^? stream .
                                               _Next .
                                                   frameValue))
