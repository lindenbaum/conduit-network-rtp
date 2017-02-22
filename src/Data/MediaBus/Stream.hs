module Data.MediaBus.Stream
    ( HasPayload(..)
    , type SourceId'
    , type SeqNum'
    , type Ticks'
    , FrameCtx(..)
    , type FrameCtx'
    , frameCtxSourceId
    , frameCtxSeqNumRef
    , frameCtxTimestampRef
    , Frame(..)
    , type Frame'
    , frameSeqNum
    , frameTimestamp
    , framePayload
    , Stream(..)
    , Stream'
    , stream
    , yieldStreamish
    , foldStreamC
    , overStreamC
    , overFramesC
    , mapFramesC
    , mapFramesC'
    , mapSeqNumC
    , mapTicksC
    , mapTicksC'
    , mapPayloadC
    , mapPayloadC'
    , convertTicksC'
    , foldStream
    , foldStreamM
    , concatStreamContents
    ) where

import           Conduit
import           Control.Monad
import           Control.Lens
import           Data.MediaBus.SourceId
import           Data.MediaBus.Sequence
import           Data.MediaBus.Payload
import           Data.MediaBus.Clock
import           Data.MediaBus.Internal.Series
import           Control.Monad.Writer.Strict   ( tell )
import           Data.Maybe
import           Data.Word
import           Test.QuickCheck
import           Data.Default
import           Text.Printf
import           GHC.TypeLits
import           GHC.Generics                  ( Generic )
import           Control.Parallel.Strategies   ( NFData, rdeepseq, withStrategy )

type SourceId' = SourceId Word32

type SeqNum' = SeqNum Word16

type Ticks' r = Ticks r Word32

data FrameCtx i s t = MkFrameCtx { _frameCtxSourceId     :: i
                                 , _frameCtxTimestampRef :: t
                                 , _frameCtxSeqNumRef    :: s
                                 }
    deriving (Eq, Ord, Generic)

instance (NFData i, NFData s, NFData t) =>
         NFData (FrameCtx i s t)

type FrameCtx' r = FrameCtx SourceId' SeqNum' (Ticks' r)

makeLenses ''FrameCtx

instance HasTimestampT (FrameCtx i s t) where
    type GetTimestamp (FrameCtx i s t) = t
    type SetTimestamp (FrameCtx i s t) t' = (FrameCtx i s t')

instance HasTimestamp (FrameCtx i s t) where
    timestamp = frameCtxTimestampRef

instance HasSeqNumT (FrameCtx i s t) where
    type GetSeqNum (FrameCtx i s t) = s
    type SetSeqNum (FrameCtx i s t) x = FrameCtx i x t

instance HasDuration (FrameCtx i s t) where
    getDuration _ = 0

instance HasSeqNum (FrameCtx i s t) where
    seqNum = frameCtxSeqNumRef

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
                           , _framePayload   :: c
                           }
    deriving (Eq, Ord, Generic)

instance (NFData c, NFData s, NFData t) =>
         NFData (Frame s t c)

deriving instance Functor (Frame s t)

type Frame' r v = Frame SeqNum' (Ticks' r) v

makeLenses ''Frame

instance HasPayload (Frame s t c) where
    type GetPayload (Frame s t c) = c
    type SetPayload (Frame s t c) d = Frame s t d
    payload = framePayload

instance HasTimestampT (Frame s t c) where
    type GetTimestamp (Frame s t c) = t
    type SetTimestamp (Frame s t c) t' = Frame s t' c

instance HasTimestamp (Frame s t c) where
    timestamp = frameTimestamp

instance HasSeqNumT (Frame s t c) where
    type GetSeqNum (Frame s t c) = s
    type SetSeqNum (Frame s t c) x = Frame x t c

instance HasSeqNum (Frame s t c) where
    seqNum = frameSeqNum

instance HasDuration c =>
         HasDuration (Frame s t c) where
    getDuration = getDuration . _framePayload

instance (Arbitrary c, Arbitrary s, Arbitrary t) =>
         Arbitrary (Frame s t c) where
    arbitrary = MkFrame <$> arbitrary <*> arbitrary <*> arbitrary

instance (Default s, Default t, Default c) =>
         Default (Frame s t c) where
    def = MkFrame def def def

instance (Show s, Show t, Show v) =>
         Show (Frame s t v) where
    show (MkFrame ts sn v) =
        printf "FRAME: %15s | %15s | %s" (show sn) (show ts) (show v)

newtype Stream i s t c = MkStream { _stream :: Streamish i s t c }
    deriving (Ord, Eq, Arbitrary, Generic)

instance (NFData i, NFData s, NFData t, NFData c) =>
         NFData (Stream i s t c)

type Streamish i s t c = Series (FrameCtx i s t) (Frame s t c)

type Stream' t c = Stream SourceId' SeqNum' (Ticks' t) c

makeLenses ''Stream

instance HasPayload (Stream i s t c) where
    type GetPayload (Stream i s t c) = c
    type SetPayload (Stream i s t c) d = Stream i s t d
    payload = stream . _Next . payload

instance HasDuration c =>
         HasDuration (Stream i s t c) where
    getDuration = maybe 0 getDuration . preview (stream . _Next)

instance HasSeqNumT (Stream i s t c) where
    type GetSeqNum (Stream i s t c) = s
    type SetSeqNum (Stream i s t c) x = Stream i x t c

instance HasSeqNum (Stream i s t c) where
    seqNum = stream . seqNum

instance HasTimestampT (Stream i s t c) where
    type GetTimestamp (Stream i s t c) = t
    type SetTimestamp (Stream i s t c) t' = Stream i s t' c

instance HasTimestamp (Stream i s t c) where
    timestamp = stream . timestamp

instance (Default c, Default s, Default t) =>
         Default (Stream i s t c) where
    def = MkStream (Next (MkFrame def def def))

instance (Show i, Show s, Show t, Show c) =>
         Show (Stream i s t c) where
    show (MkStream s) = show s

yieldStreamish :: Monad m => Streamish i s t c -> Source m (Stream i s t c)
yieldStreamish = yield . MkStream

foldStreamC :: Monad m
            => (StartingFrom (FrameCtx i s t) -> Conduit (Frame s t c) m o)
            -> Conduit (Stream i s t c) m o
foldStreamC = mapInput _stream (Just . MkStream) . foldSeriesC

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

mapFramesC' :: (NFData i, NFData s, NFData t, NFData c', Monad m)
            => (Frame s t c -> m (Frame s t c'))
            -> Conduit (Stream i s t c) m (Stream i s t c')
mapFramesC' f = mapMC (mapMOf (stream . _Next) f >=>
                           return . withStrategy rdeepseq)

mapFramesC :: Monad m
           => (Frame s t c -> m (Frame s t c'))
           -> Conduit (Stream i s t c) m (Stream i s t c')
mapFramesC f = mapMC (mapMOf (stream . _Next) f)

mapSeqNumC :: Monad m
           => (s -> s')
           -> Conduit (Stream i s t c) m (Stream i s' t c)
mapSeqNumC = mapC . over seqNum

mapTicksC :: Monad m
          => (t -> t')
          -> Conduit (Stream i s t c) m (Stream i s t' c)
mapTicksC = mapC . over timestamp

mapTicksC' :: (NFData t, Monad m)
           => (t -> t')
           -> Conduit (Stream i s t c) m (Stream i s t' c)
mapTicksC' = mapC . withStrategy rdeepseq . over timestamp

mapPayloadC :: Monad m
            => (c -> m c')
            -> Conduit (Stream i s t c) m (Stream i s t c')
mapPayloadC = mapMC . mapMOf payload

mapPayloadC' :: (NFData (Stream i s t c'), Monad m)
             => (c -> m c')
             -> Conduit (Stream i s t c) m (Stream i s t c')
mapPayloadC' f = mapMC (mapMOf payload f >=> return . withStrategy rdeepseq)

convertTicksC' :: forall proxy0 proxy1 m r t r' t' i s c.
              (NFData t, NFData t', KnownNat r, KnownNat r', Integral t, Integral t', Monad m, NFData t')
              => proxy0 '(r, t)
              -> proxy1 '(r', t')
              -> Conduit (Stream i s (Ticks r t) c) m (Stream i s (Ticks r' t') c)
convertTicksC' _ _ = mapTicksC' convertTicks

foldStream :: (Monoid o, Monad m)
           => (Stream i s t c -> o)
           -> Sink (Stream i s t c) m o
foldStream f = execWriterC $
    awaitForever $
        tell .
            f

foldStreamM :: (Monoid o, Monad m)
            => (Stream i s t c -> m o)
            -> Sink (Stream i s t c) m o
foldStreamM f = execWriterC $
    awaitForever (lift . lift . f >=> tell)

concatStreamContents :: (Monoid c, Monad m) => Sink (Stream i s t c) m c
concatStreamContents = foldStream (fromMaybe mempty .
                                       (^? stream .
                                               _Next .
                                                   framePayload))
