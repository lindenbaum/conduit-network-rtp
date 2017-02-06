{-# LANGUAGE UndecidableInstances #-}

module Data.MediaBus.Basics
    ( SourceId(..)
    , sourceId
    ) where

import           Control.Lens

--
-- | Things that can be uniquely identified by a looking at a (much simpler)
-- representation, the 'identity'.
data SourceId i = MkSourceId { _sourceId :: i }

makeLenses ''SourceId

instance Show i => Show (SourceId i) where
  show (MkSourceId x) = "ID: " ++ show x

{-
TODOs

0. Basics on the API
  -- newtype Frame i s t c = MkFrame { _frame :: IdentifiedBy i (SequenceNumbered s (Sync t c)) }
  --
  -- data FrameCtx i s t = MkFrameCtx { frameSourceId :: i, frameSeqNum :: s, frameSeqNumRef :: s, fromeTimestamp :: t, frameTimestampRef :: t}
  --
  -- type FrameContentC i s t c c' m r = ConduitM c c' (ReaderT (FramceCtx i s t) m) r
  --
  -- type FrameC i s t c c' m r = ConduitM (Frame i s t c) (Frame i s t c') m r
  --
  -- overFrameContentC :: Monad m => FrameContentC i s t c c' m r -> FrameC i s t c c' m r
  --
  ---
  --
  -- Stream a b c =   MkStreamStrart a | MkStream b
  --                          | MkStreamEnd c
  --
  -- data StreamCtx a = StreamIdle | StreamStarted { streamStartArg :: a }
  --
  -- type StreamContentC a b b' m o = ConduitM b b' (ReaderT (StreamCtx a) m) o
  --
  -- type StreamC a b b' m o = ConduitM (Stream a b o) (Stream a b' o) m o
  --
  -- overStreamContentC :: Monad m => StreamContentC a b b' m o -> StreamC a b b' m o
  --
  ---
  --
  -- type FrameStreamC a i s t c c' m o = StreamC a (Frame i s t c) (Frame i s t c') m o
  --
  -- overFrameStreamContent :: Monad m => FrameContentC i s t c c' m r -> FrameStreamC a i s t c c' m
  --
  -- wenn bei einigen events z.B. SetNewId eine neue Id gesetzt wird sollen interessierte stellen sich das
  -- halt merken und ansonsten den Id aspekt ignorieren, wie z.b. auch beim Sync aspekt, schlimmstenfalls macht man ein MonadReader oder so...
  --
  -- Achja ...der Grund weshalb man nicht einen "komplett-flachen" Frame Typ haben sollte liegt in der unabhaengigkeit der
  -- sich innerhalb eines logischen Streams aendernden aspekte. Wenn sich z.B. die SSRC aendert, welcher Timestamp sollte dann gesetzt sein?
  -- Wiederholt sich der Timestamp dann nochmal, wenn dann die payload daten kommen?? Wie sollen Jitterbuffer etc. damit umgehen?
  -- Daher lieber so: Es gibt eine hierachy von meta- und payloaddaten, wenn ein meta datum ein Summentyp ist, wie z.B. IdentifiedBy, dann
  -- soll der ganze Rattenschwanz dahinter (timestamps, sequence number und letzendlich payload data) einfach mal ausbleiben.
  --
  -- Dann kann man schoene kombinatoren bauen, die sich den neuesten wert merken und in einen ReaderT stecken, oder welche, die sich
  -- wie Funktoren benehmen....
  -- man kann auch Prism aehnliche Dinger nehmen um auf die einzelnen events zu reagieren:
  -- >>> onResync :: Monad m => ((Ticks timing) -> m ()) -> FrameResampler s s timing timing m
  -- >>> overSyncedFrames :: Monad m => ((Ticks timing) -> m

1. Move&Clean RtpPacket/RtpSource modules

2. RTP pipeline:
   - udp
   - parse -> type RawRtpFrame = RtpFrame (RtpPayload pt (SampleBuffer RtpRawContent))
              type RtpPayloadFrame rtpPt = RtpFrame (SampleBuffer rtpPt)
   -          type RtpFrame x = Frame RtpSsrc RtpSeqNum RtpTS x
   - demultiplex on PayloadType: demuxPT :: Map RtpPT (ConduitM RawRtpFrame (RtpPayloadFrame ))
   - on ssrc change: StreamStart
   - >>> smooth seqnumber <- if delta seqnum > 10 then yieldRestartSequence seqnum payload else yieldSeqNum seqnum payload
   - >>> await >>= (over _SeqNumRestart resetRingBuffer . over _SeqNum addToRingBuffer)
   - sort by rtp timestamp (on streamStart )
   -

-}
