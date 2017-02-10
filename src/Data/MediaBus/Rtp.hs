module Data.MediaBus.Rtp
    ( type RtpStream
    , rtpSource
    ) where

import           Conduit
import           Control.Lens
import qualified Data.ByteString               as B
import           Data.MediaBus.Internal.Series
import qualified Data.MediaBus.Rtp.Packet      as Rtp
import           Data.MediaBus.Stream
import           Control.Monad
import           Data.Default
import           Text.Printf
import           Debug.Trace

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

data RtpPayloadContext = MkRtpPayloadContext

type RtpOutStream = Stream Rtp.RtpSsrc Rtp.RtpSeqNum Rtp.RtpTimestamp B.ByteString
