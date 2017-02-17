module Data.MediaBus.Rtp
    ( type RtpStream
    , rtpSource
    ) where

import           Conduit
import           Control.Lens
import qualified Data.ByteString               as B
import           Data.MediaBus.Audio.Alaw
import           Data.MediaBus.Clock
import           Data.MediaBus.Sample
import           Data.MediaBus.Stream
import           Data.MediaBus.Internal.Series
import qualified Data.MediaBus.Rtp.Packet      as Rtp
import           Control.Monad
import           Data.Default
import           Text.Printf
import           Debug.Trace
import           Data.Word
import           Data.Coerce

type RtpStream = Stream Rtp.RtpSsrc Rtp.RtpSeqNum Rtp.RtpTimestamp Rtp.RtpPayload

data RRState ctx = MkRRState { _currCtx       :: ctx
                             , _isFirstPacket :: Bool
                             }

makeLenses ''RRState

rtpSource :: (Monad m) => Conduit (Stream i s t B.ByteString) m RtpStream
rtpSource = foldStreamC $
    \(MkStartingFrom _originalFrameCtx) ->
        evalStateC (MkRRState (MkFrameCtx def def def) True) $
            awaitForever processFrames
  where
    processFrames (MkFrame _ _ contentIn) =
        case Rtp.deserialize contentIn of
            Left rtpError -> traceRtpError rtpError
            Right rtpPacket -> do
                let rtpHeader = Rtp.header rtpPacket
                wasFirst <- use isFirstPacket
                (oldCtx, res) <- updateState rtpHeader
                when (res == SourceHasChanged)
                     (traceSourceChangeM wasFirst oldCtx >> yieldStreamStart)
                yieldStreamNext (Rtp.body rtpPacket)

    traceRtpError e = do
        ctx <- use currCtx
        traceM (printf "RTP-ERROR:%s  Ctx: %s\n" e (show ctx))

    traceSourceChangeM isFirst oldCtx = do
        newCtx <- use currCtx
        if isFirst
            then traceM (printf "RTP-START:\n  Ctx: %s\n" (show newCtx))
            else traceM (printf "RTP-RESTART:\n  Old-Ctx: %s\n  New-Ctx: %s\n"
                                (show oldCtx)
                                (show newCtx))

    updateState rtpHeader = do
        oldCtx <- currCtx <<%=
                      ((frameCtxSeqNumRef .~ Rtp.sequenceNumber rtpHeader)
                           . (frameCtxTimestampRef .~
                                  Rtp.timestamp rtpHeader))
        wasFirstPacket <- isFirstPacket <<.= False
        if oldCtx ^. frameCtxSourceId /= Rtp.ssrc rtpHeader
            then do
                currCtx . frameCtxSourceId .= Rtp.ssrc rtpHeader
                return (oldCtx, SourceHasChanged)
            else if sequenceNumbersDifferTooMuch (oldCtx ^. frameCtxSeqNumRef)
                                                 (Rtp.sequenceNumber rtpHeader) ||
                     timestampsDifferTooMuch (oldCtx ^. frameCtxTimestampRef)
                                             (Rtp.timestamp rtpHeader) ||
                     wasFirstPacket
                 then return (oldCtx, SourceHasChanged)
                 else return (oldCtx, SourceHasNotChanged)
      where
        sequenceNumbersDifferTooMuch oldSN currSN =
            let d = if currSN >= oldSN then currSN - oldSN else oldSN - currSN
                sequenceNumberMaxDelta =
                    10
            in
                d >= sequenceNumberMaxDelta
        timestampsDifferTooMuch oldTS currTS =
            let d = if currTS >= oldTS then currTS - oldTS else oldTS - currTS
                timestampMaxDelta = 2000
            in
                d >= timestampMaxDelta
    yieldStreamStart = use currCtx >>= yield . MkStream . Start
    yieldStreamNext p = do
        ts <- use (currCtx . frameCtxTimestampRef)
        sn <- use (currCtx . frameCtxSeqNumRef)
        yield (MkStream (Next (MkFrame ts sn p)))

data RRSourceChange = SourceHasChanged | SourceHasNotChanged
    deriving (Eq)

type RtpOutStream = Stream Rtp.RtpSsrc Rtp.RtpSeqNum Rtp.RtpTimestamp B.ByteString

-- TODO use the SourceId as a enriched stream config type, which contains the ssrc but also ptime, etc...
rtpPayloadDemux :: (Monad m)
                =>
                 proxy rate
                -> [(Word8, PayloadHandler m (Ticks r w) out)]
                -> Conduit RtpStream m (Stream i s (Ticks r w) out)
rtpPayloadDemux timing payloadTable =
    undefined

type PayloadHandler m t c = Conduit RtpStream m (Stream Rtp.RtpSsrc Rtp.RtpSeqNum t c)



alawPayloadHandler :: Monad m
                   => Conduit RtpStream m (Stream Rtp.RtpSsrc Rtp.RtpSeqNum (Ticks' 8000) (SampleBuffer ALaw))
alawPayloadHandler = mapC ((timestamp %~ (MkTicks . Rtp._rtpTimestamp))
                               . (payload %~ (coerce . Rtp._rtpPayload)))




-- TODO: Add a DTX stream state, indicating a silence period. The packet rate may drop during silence! Silence might begin with the reception of the first comfort noise packet, e.g. with payload type 13, see https://tools.ietf.org/html/rfc3389 and https://tools.ietf.org/html/rfc3551#section-4.1

-- TODO: split up the received data into equally sized chunks? - leave that to the application

-- TODO: drop duplicate packets

-- TODO: add parameters: channel layout, bit rate, ptime, maxptime
