module Data.MediaBus.Applications.RtpAlawAudio
    ( periodicRtpAlawUdpReceiver16kHzS16OrSilence
    , rtpAlawUdpReceiver16kHzS16
    ) where

import           Conduit
import           Control.Monad                       ( void )
import           Data.MediaBus.Audio.Raw
import           Data.MediaBus.Audio.Resample
import           Data.MediaBus.BlankMedia
import           Data.MediaBus.Clock
import           Data.MediaBus.Discontinous
import           Data.MediaBus.Packetizer
import           Data.MediaBus.Reorder
import           Data.MediaBus.Rtp
import           Data.MediaBus.Rtp.Packet
import           Data.MediaBus.Sample
import           Data.MediaBus.Stream
import           Data.MediaBus.SourceId
import           Data.MediaBus.Internal.Conduit
import           Data.MediaBus.Transcoder
import           Data.MediaBus.Transport.Udp
import           Data.MediaBus.Internal.AsyncConduit
import           Data.Streaming.Network              ( HostPreference )
import           Data.Time.Clock
import           Data.Word
import           Control.Concurrent.Async.Lifted
import           Data.Proxy
import qualified Data.ByteString                     as B
import           Network.Socket                      ( SockAddr )

periodicRtpAlawUdpReceiver16kHzS16OrSilence :: (MonadResource m, MonadBaseControl IO m)
                                            => Int
                                            -> HostPreference
                                            -> NominalDiffTime
                                            -> Int
                                            -> m ( Async ()
                                                 , Source m (Stream RtpSsrc RtpSeqNum (Ticks 16000 Word64) (SampleBuffer (S16 16000)))
                                                 )
periodicRtpAlawUdpReceiver16kHzS16OrSilence !udpListenPort !udpListenHostIP !ptime !frameQLength = do
    let !pollIntervall = 0.5 * fromIntegral frameQLength * ptime
    (!asrc, !src) <- mkDecoupledSource frameQLength
                                       pollIntervall
                                       ptime
                                       (rtpAlawUdpReceiver16kHzS16 udpListenPort
                                                                   udpListenHostIP
                                                                   ptime
                                                                   frameQLength)

    return (void asrc, src .| concealMissing blankFor)

rtpAlawUdpReceiver16kHzS16 :: MonadResource m
                           => Int
                           -> HostPreference
                           -> NominalDiffTime
                           -> Int
                           -> Source m (Stream RtpSsrc RtpSeqNum (Ticks 16000 Word64) (SampleBuffer (S16 16000)))
rtpAlawUdpReceiver16kHzS16 !udpListenPort !udpListenIP !ptime !reorderBufferSize =
    annotateTypeSource (Proxy :: Proxy (Stream (SourceId (Maybe SockAddr)) RtpSeqNum (TimeDiff UtcClock) B.ByteString))
                       (udpDatagramSource useUtcClock udpListenPort udpListenIP) .|
        rtpSource .|
        rtpPayloadDemux [ (8, alawPayloadHandler) ] mempty .|
        transcodeStreamC' .|
        resample8to16kHz' (MkS16 0 :: S16 8000) .|
        convertTicksC' at8kHzU32 at16kHzU64 .|
        reorderFramesBySeqNumC reorderBufferSize .|
        repacketizeC ptime
