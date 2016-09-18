-- | A conduit that listens to a UDP port for RTP with payload type @8@ which
-- mean /G.711 A-law/ and orders and converts the packets to 'Pcm'
--
-- This is a __top-level__ module, it re-exports many other modules.
module Data.Conduit.Audio.RtpAlawSource
  ( rtpAlaw16kSource, rtpAlawToPcm16k, rtpAlaw8kSource, rtpAlawToPcm8k, module X ) where

import           Data.Conduit.Audio.Alaw
import           Data.Conduit.Audio.Pcm       as X
import           Data.Conduit.Audio.Reorder   as X
import           Data.Conduit.Audio.RtpSource as X

-- | Listen to incoming RTP packets on a UDP port and convert them to 'Pcm' packets.
-- The 8k sample rate is automatically converted to 16k.
rtpAlaw16kSource
  :: MonadResource m
  => Int
  -> HostPreference
  -> Source m (RtpEvent Pcm16KMono)
rtpAlaw16kSource !port !host =
  udpSession port host =$= reorder =$= rtpAlawToPcm16k

-- | Convert RTP packets to 'Pcm' packets.
-- The 8k sample rate is automatically converted to 16k.
rtpAlawToPcm16k
  :: Monad m
  => Conduit RtpEventRaw m (RtpEvent Pcm16KMono)
rtpAlawToPcm16k = go 0
  where
    go !lastVal =
      do  me <- await
          case me of
            Nothing -> return ()
            Just (InBand (SequenceOf !s (NoGap (Packet Header{..} !body))))
              | payloadType == 8 ->
                let (!lastVal', !pcm) = alawToLinear16k lastVal (Alaw body)
                in yield (InBand (SequenceOf s (NoGap pcm))) >> go lastVal'
              | otherwise ->
                yield (InBand (SequenceOf s Gap)) >> go lastVal
            Just (InBand (SequenceOf !s Gap)) ->
              yield (InBand (SequenceOf s Gap)) >> go lastVal
            Just (OutOfBand !b) ->
              yield (OutOfBand b) >> go lastVal

-- | Listen to incoming RTP packets on a UDP port and convert them to 'Pcm' packets.
rtpAlaw8kSource
  :: MonadResource m
  => Int
  -> HostPreference
  -> Source m (RtpEvent Pcm8KMono)
rtpAlaw8kSource !port !host =
  udpSession port host =$= reorder =$= rtpAlawToPcm8k

-- | Convert RTP packets to 'Pcm' packets.
rtpAlawToPcm8k
  :: Monad m
  => Conduit RtpEventRaw m (RtpEvent Pcm8KMono)
rtpAlawToPcm8k = awaitEventForever go yieldInbandGap yieldOutOfBand
  where
    go !s (Packet Header{payloadType} !body) =
      if (payloadType == 8)
      then yieldInband s (alawToLinear (Alaw body))
      else yieldInbandGap s
