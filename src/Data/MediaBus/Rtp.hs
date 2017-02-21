{-# OPTIONS -Wno-unused-top-binds #-}

module Data.MediaBus.Rtp
    ( type RtpStream
    , rtpSource
    , rtpPayloadDemux
    , type RtpPayloadHandler
    , alawPayloadHandler
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
import           Data.Coerce
import qualified Data.List
import           Data.Maybe

type RtpStream = Stream Rtp.RtpSsrc Rtp.RtpSeqNum Rtp.RtpTimestamp Rtp.RtpPayload

data RRState ctx = MkRRState { _currCtx       :: ctx
                             , _isFirstPacket :: Bool
                             }
    deriving (Show)

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
                res <- updateState rtpHeader
                when (res == FrameCtxChanged) yieldStreamStart
                yieldStreamNext (Rtp.body rtpPacket)

    traceRtpError e = do
        ctx <- use currCtx
        traceM (printf "RTP-ERROR:%s  Ctx: %s\n" e (show ctx))

    updateState rtpHeader = do
        oldCtx <- currCtx <<%=
                      ((frameCtxSeqNumRef .~ Rtp.sequenceNumber rtpHeader)
                           . (frameCtxTimestampRef .~ Rtp.timestamp rtpHeader))
        wasFirstPacket <- isFirstPacket <<.= False
        if oldCtx ^. frameCtxSourceId /= Rtp.ssrc rtpHeader
            then do
                currCtx . frameCtxSourceId .= Rtp.ssrc rtpHeader
                return FrameCtxChanged
            else if sequenceNumbersDifferTooMuch (oldCtx ^. frameCtxSeqNumRef)
                                                 (Rtp.sequenceNumber rtpHeader) ||
                     timestampsDifferTooMuch (oldCtx ^. frameCtxTimestampRef)
                                             (Rtp.timestamp rtpHeader) ||
                     wasFirstPacket
                 then return FrameCtxChanged
                 else return FrameCtxNotChanged
      where
        sequenceNumbersDifferTooMuch oldSN currSN =
            let d = if currSN >= oldSN then currSN - oldSN else oldSN - currSN -- TODO use LocalOrd??
                sequenceNumberMaxDelta =
                    10
            in
                d >= sequenceNumberMaxDelta
        timestampsDifferTooMuch oldTS currTS =
            let d = if currTS >= oldTS then currTS - oldTS else oldTS - currTS
                timestampMaxDelta = 2000 -- TODO extract
            in
                d >= timestampMaxDelta
    yieldStreamStart = use currCtx >>= yield . MkStream . Start
    yieldStreamNext p = do
        ts <- use (currCtx . frameCtxTimestampRef)
        sn <- use (currCtx . frameCtxSeqNumRef)
        yield (MkStream (Next (MkFrame ts sn p)))

data RRSourceChange = FrameCtxChanged | FrameCtxNotChanged
    deriving (Eq)

rtpPayloadDemux :: (Integral t, Monad m)
                => [(Rtp.RtpPayloadType, RtpPayloadHandler m (Ticks r t) c)]
                -> c
                -> Conduit RtpStream m (Stream Rtp.RtpSsrc Rtp.RtpSeqNum (Ticks r t) c)
rtpPayloadDemux payloadTable fallbackContent =
    mapC (timestamp %~ (MkTicks . fromIntegral . Rtp._rtpTimestamp)) .|
        overFramesC go
  where
    setFallbackContent = return . (payload .~ fallbackContent)
    go _start = awaitForever handleFrame
      where
        handleFrame frm = let pt = frm ^. framePayload . Rtp.rtpPayloadType
                              mHandler = Data.List.lookup pt payloadTable
                          in
                              lift (fromMaybe setFallbackContent mHandler frm) >>=
                                  yield

type RtpPayloadHandler m t c = Frame Rtp.RtpSeqNum t Rtp.RtpPayload
    -> m (Frame Rtp.RtpSeqNum t c)

alawPayloadHandler :: Monad m => RtpPayloadHandler m t (SampleBuffer ALaw)
alawPayloadHandler = return . (payload %~ (coerce . Rtp._rtpPayload))
