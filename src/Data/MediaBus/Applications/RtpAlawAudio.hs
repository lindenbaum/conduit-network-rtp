module Data.MediaBus.Applications.RtpAlawAudio ( rtpAlawUdpReceiver16kHzS16 ) where

import           Conduit
import           Data.MediaBus.Audio.Raw
import           Data.MediaBus.Audio.Resample
import           Data.MediaBus.Clock
import           Data.MediaBus.Packetizer
import           Data.MediaBus.Reorder
import           Data.MediaBus.Rtp
import           Data.MediaBus.Rtp.Packet
import           Data.MediaBus.Sample
import           Data.MediaBus.Stream
import           Data.MediaBus.SourceId
import           Data.MediaBus.Conduit
import           Data.MediaBus.Transcoder
import           Data.MediaBus.Transport.Udp
import           Data.Streaming.Network          ( HostPreference )
import           Data.Time.Clock
import           Data.Word
import           Data.Proxy
import qualified Data.ByteString                 as B
import           Network.Socket                  ( SockAddr )

rtpAlawUdpReceiver16kHzS16 :: MonadResource m
                           => Int
                           -> HostPreference
                           -> NominalDiffTime
                           -> Int
                           -> Source m (Stream RtpSsrc RtpSeqNum (Ticks 16000 Word64) (SampleBuffer (S16 16000)))
rtpAlawUdpReceiver16kHzS16 !udpListenPort !udpListenIP !ptime !reorderBufferSize =
    annotateTypeSource (Proxy :: Proxy (Stream (SourceId (Maybe SockAddr)) RtpSeqNum (ClockTimeDiff UtcClock) B.ByteString))
                       (udpDatagramSource useUtcClock udpListenPort udpListenIP) .|
        rtpSource .|
        rtpPayloadDemux [ (8, alawPayloadHandler) ] mempty .|
        transcodeStreamC' .|
        resample8to16kHz' (MkS16 0 :: S16 8000) .|
        convertTicksC' at8kHzU32 at16kHzU64 .|
        reorderFramesBySeqNumC reorderBufferSize .|
        repacketizeC ptime
