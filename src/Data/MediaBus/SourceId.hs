module Data.MediaBus.SourceId
    ( SourceId(..)
    , sourceId
    ) where

import           Control.Lens
import           Test.QuickCheck
import           Data.Default

--
-- | Things that can be uniquely identified by a looking at a (much simpler)
-- representation, the 'identity'.
newtype SourceId i = MkSourceId { _sourceId :: i }
    deriving (Eq, Arbitrary, Default, Ord)

makeLenses ''SourceId

instance Show i =>
         Show (SourceId i) where
    show (MkSourceId x) = "SOURCE-ID: " ++ show x{-
TODOs

0. Basics on the API
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
