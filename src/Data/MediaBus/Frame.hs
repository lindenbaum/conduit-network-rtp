module Data.MediaBus.Frame
    ( Frame(..)
    , frame
    , type SourceId'
    , type SeqNum'
    , type Ticks'
    , type Timing'
    , type Frame'
    , type FrameBuffer
    , type FrameBuffer'
    , FrameCtx(..)
    , frameCtxSourceId
    , frameCtxSeqNumRef
    , frameCtxTimestampRef
    , type FrameCtx'
    , FrameValue(..)
    , type FrameValue'
    , frameSeqNum
    , frameTimestamp
    , frameValue
    , type FrameC
    , type FrameC'
    , type FrameContentC
    , type FrameContentC'
    , Transcoder(..)
    , overFrameContentC
    , frameResamplerM
    , type FrameSink
    , type FrameSink'
    , foldFrames
    , foldFramesM
    , concatFrameContents
    , dbgShowFrameC
    ) where

import           Conduit
import           Control.Monad
import           Control.Lens
import           Data.MediaBus.Sample
import           Data.MediaBus.Basics
import           Data.MediaBus.Sequence
import           Data.MediaBus.Clock
import           Data.MediaBus.Internal.Relative
import           Control.Monad.Writer.Strict     ( tell )
import           Control.Monad.State.Strict      as State
import           Debug.Trace
import           System.Random
import           Data.Maybe
import           Data.Word

-- | A 'Frame' can be anything that has a start time and is exactly one time
-- unit long, it can respresent anything ranging from an audio buffer with 20ms
-- of audio to a single pulse coded audio sample, of course it could also be a
-- video frame or a chat message.
newtype Frame i s t c = MkFrame { _frame :: Relative i (Series s (SequenceNumbered s (Series t (Sync t c))))
                                }
    deriving (Functor)

type SourceId' = SourceId Word32

type SeqNum' = SeqNum Word16

type Timing' r = Timing r Word32

type Ticks' r = Ticks (Timing' r)

type Frame' r c = Frame SourceId' SeqNum' (Ticks' r) c

instance (Show i, Show s, Show t, Show c) =>
         Show (Frame i s t c) where
    show (MkFrame x) = "«" ++ show x ++ "»"

type FrameBuffer i s t c = Frame i s t (SampleBuffer c)

type FrameBuffer' r c = FrameBuffer SourceId' SeqNum' (Ticks' r) (SampleBuffer c)

makeLenses ''Frame

data FrameCtx i s t = MkFrameCtx { _frameCtxSourceId     :: i
                                 , _frameCtxSeqNumRef    :: s
                                 , _frameCtxTimestampRef :: t
                                 }
    deriving Eq

type FrameCtx' r = FrameCtx SourceId' SeqNum' (Ticks' r)

instance (Show i, Show s, Show t) =>
         Show (FrameCtx i s t) where
    show (MkFrameCtx sid snr tsr) =
        "{{ source: " ++
            show sid ++
                ", sn-ref: " ++
                    show snr ++
                        ", ts-ref: " ++
                            show tsr ++
                                " }}"

makeLenses ''FrameCtx

data FrameValue s t c = MkFrameValue { _frameSeqNum    :: s
                                     , _frameTimestamp :: t
                                     , _frameValue     :: c
                                     }
    deriving Eq

type FrameValue' r v = FrameValue SeqNum' (Ticks' r) v

instance (Show s, Show t, Show v) =>
         Show (FrameValue s t v) where
    show (MkFrameValue sn ts v) =
        "FRAME sn: " ++
            show sn ++
                ", ts: " ++
                    show ts ++
                        ", v: " ++
                            show v

makeLenses ''FrameValue

type FrameC i s t c c' m r = ConduitM (Frame i s t c) (Frame i s t c') m r

type FrameC' t c c' m r = FrameC SourceId' SeqNum' (Ticks' t) c c' m r

type FrameContentC i s t c c' m r = FrameCtx i s t
    -> ConduitM (FrameValue s t c) c' m r

type FrameContentC' t c c' m r = FrameContentC SourceId' SeqNum' (Ticks' t) c c' m r

class Transcoder from to i s t m where
    transcode :: FrameContentC i s t from to m ()

overFrameContentC :: Monad m
                  => i
                  -> FrameContentC i s t c c' m ()
                  -> FrameC i s t c c' m ()
overFrameContentC i0 f =
    mapInput _frame (Just . MkFrame) (mapOutput MkFrame process)
  where
    process = overRelativeFirstC (const i0)
        $ \i -> overSeriesC _seqNum
            $ \(MkStartingFrom snr) ->
                overSequenceNumberedC $
                    \sn -> overSeriesC _syncTimestamp
                        $ \(MkStartingFrom tsr) ->
                            let fc = MkFrameCtx i snr tsr
                            in
                                overSyncC $
                                    \ts -> mapInput (MkFrameValue sn ts)
                                                    (const Nothing)
                                                    (f fc)

frameResamplerM :: forall i s t t' c c' m.
                Monad m
                => (t -> t')
                -> (c -> m c')
                -> ConduitM (Frame i s t c) (Frame i s t' c') m ()
frameResamplerM fTicks fContent =
    awaitForever (doContent . doTicks >=> yield)
  where
    doContent = lift .
        mapMOf (frame .
                    _MkRelative .
                        _Next . seqNumValue . _Next . syncValue)
               fContent
    doTicks = over (frame . _MkRelative . _Next . seqNumValue)
                   (over _Start fTicks
                        . over (_Next . syncTimestamp) fTicks)

type FrameSink i s t c m r = Sink (Frame i s t c) m r

type FrameSink' t c m r = FrameSink SourceId' SeqNum' (Ticks' t) c m r

foldFrames :: (Monoid o, Monad m)
           => (Frame i s t c -> o)
           -> FrameSink i s t c m o
foldFrames f = execWriterC $
    awaitForever $
        tell .
            f

foldFramesM :: (Monoid o, Monad m)
            => (Frame i s t c -> m o)
            -> FrameSink i s t c m o
foldFramesM f = execWriterC $
    awaitForever (lift . lift . f >=> tell)

concatFrameContents :: (Monoid c, Monad m) => FrameSink i s t c m c
concatFrameContents = foldFrames (fromMaybe mempty .
                                      (^? frame .
                                              _MkRelative .
                                                  _Next .
                                                      seqNumValue .
                                                          _Next .
                                                              syncValue))

dbgShowFrameC :: (Show (Frame i s t c), Monad m)
              => Double
              -> String
              -> FrameC i s t c c m ()
dbgShowFrameC probability msg =
    evalStateC (mkStdGen 100, 0 :: Integer) $
        awaitForever $
            \x -> do
                (g, omitted) <- State.get
                let (p, g') = randomR (0, 1) g
                if p < probability
                    then do
                        let prefix = if omitted == 0
                                     then ""
                                     else "(" ++
                                         show omitted ++
                                             " messages omitted) "
                        traceM (prefix ++ msg ++ ": " ++ show x)
                        State.put (g', 0)
                    else State.put (g', omitted + 1)
                yield x
