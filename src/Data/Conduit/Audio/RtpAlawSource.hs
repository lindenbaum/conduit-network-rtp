{-# LANGUAGE NamedFieldPuns #-}
-- | A conduit that listens to a UDP port for RTP with payload type @8@ which
-- mean /G.711 A-law/ and orders and converts the packets to 'Pcm'
--
-- This is a __top-level__ module, it re-exports many other modules.
module Data.Conduit.Audio.RtpAlawSource
  ( rtpAlaw16kSource, rtpAlawToPcm16k, module X ) where

import           Data.Conduit.Audio.Alaw
import           Data.Conduit.Audio.Pcm       as X
import           Data.Conduit.Audio.Reorder   as X
import           Data.Conduit.Audio.RtpSource as X

import System.IO (Handle)
import System.Process (shell)
import Data.Streaming.Process
import Data.Conduit.Binary
import Data.Vector.Storable.ByteString

-- | Listen to incoming RTP packets on a UDP port and convert them to 'Pcm' packets.
-- The 8k sample rate is automatically converted to 16k.
rtpAlaw16kSource
  :: MonadResource m
  => Int
  -> HostPreference
  -> Source m (RtpEvent Pcm16KMono)
rtpAlaw16kSource !port !host =
  udpSession port host =$= reorder =$= rtpAlawToPcm16k

rtpAlawToPcm16k
  :: Monad m
  => Conduit RtpEventRaw m (RtpEvent Pcm16KMono)
rtpAlawToPcm16k = awaitEventForever go yieldInbandGap yieldOutOfBand
  where
    go !s (Packet Header{payloadType} !body) =
      when (payloadType == 8) (yieldInband s (alawToLinear16K (Alaw body)))


dbgSoundCardSink
  :: MonadIO m => Sink (RtpEvent Pcm16KMono) m ()
dbgSoundCardSink = do
  let cp = shell "play -x -r 16000 -b 16 -c1  -e signed-integer -t raw -"
  (sin :: Handle, Inherited, Inherited, cph) <- streamingProcess cp
  awaitForever pcmToByteString =$= sinkHandle sin
  waitForStreamingProcess cph
  return ()
  where
    pcmToByteString (InBand (SequenceOf s (NoGap (Pcm !d)))) = yield (vectorToByteString d)
    pcmToByteString _ = return ()
