-- | Receive an RTP stream from the network using UDP,
--
-- An RTP session is maintained, containing the SSRC, sequence number and
-- relative timestamps and sequence numbers.
module Data.Conduit.Audio.RtpSource ( ) where

import Data.Conduit.Audio.RtpPacket (Packet(..), Header(..), deserialize)
import qualified Data.ByteString as B
import           Data.Conduit
import           Data.Conduit.Audio.RtpPacket
import           Data.Conduit.Audio.Event
import           Data.Conduit.Network.UDP
import           Data.Streaming.Network
import           Network.Socket               (close)
import           Text.Printf
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Word

-- | A UDP source that uses 'MonandResource' to make sure the socket is closed.
udpSource
  :: MonadResource m
  => Int
  -> HostPreference
  -> Source m Message
udpSource !port !host = do
  bracketP (bindPortUDP port host) close
    (\sock -> sourceSocket sock 1024)

-- | Extract a 'B.ByteString' from a 'Message'
parseUdpMessages
  :: MonadIO m
  => Conduit Message m B.ByteString
parseUdpMessages =
  awaitForever (yield . msgData)

-- | Receive messages and parse them as 'Packet's
parsePackets
  :: Monad m
  => Conduit B.ByteString m Packet
parsePackets =
  awaitForever (yield . deserialize)

packetDbgSink
  :: MonadIO m
  => Sink Packet m ()
packetDbgSink =
  awaitForever $
  \ (Packet Header{..} _) ->
    liftIO $ do
      when hasMarker (putStrLn "         !!!")
      printf "SSRC: %0.4x | TS: %0.4x | SEQ: %d\n" ssrc timestamp (unSeqNum sequenceNumber)

-- | The in and out of band message for RTP
type RtpEvent body = Event (PacketMsg body) SessionMsg
type RtpEventRaw = Event (PacketMsg B.ByteString) SessionMsg

-- | A new packet to process
data PacketMsg body =
  PacketMsg { pmPayloadType :: !Word8
            , pmBody :: !body }

-- | The type of message that are send by 'session'.
data SessionMsg =
   BeginSession !SessionInfo
 | EndSession

data SessionInfo =
  SessionInfo { siSsrc :: !Word32
              , siStartSeq :: !SeqNum
              , siStartTimeStamp :: !Word32
              }

-- | Now something interesting. A monadic bind of consumers will sequence the
-- conduits. We start a new RTP Session by first waiting for the first packet,
-- then pass control over to a conduit that knows the SSRC and exits when
-- another SSRC comes. The first packet also sends events for the 'JitterBuffer'
-- and the audio decoder.
session
  :: MonadIO m
  => Conduit Packet m RtpEventRaw
session = (initiateSession >>= mapM_ continueSession)

-- | Wait for the first RTP packet and send 'Beginsession'.
initiateSession
  :: Monad m
  => ConduitM Packet RtpEventRaw m (Maybe SessionInfo)
initiateSession = do
  mp <- await
  case mp of
    Nothing ->
      return Nothing
    Just p@(Packet Header{..} !body) ->
      do let !si = SessionInfo ssrc sequenceNumber timestamp
         yieldOutOfBand (BeginSession si)
         leftover p
         return (Just si)

-- | Convert 'Packet' messages to 'RtpEvent's until the ssrc changes, in that case
-- send a 'EndSession' 'OutOfBand'. TODO Add RTCP support, RTCP defines /BYE/ messages.
continueSession
  :: Monad m
  => SessionInfo -> Conduit Packet m RtpEventRaw
continueSession SessionInfo{..} = awaitForever go
  where
    go (Packet Header{..} !body) =
      if ssrc /= siSsrc
      then
        yieldOutOfBand EndSession
      else
        yieldInband (sequenceNumber - siStartSeq) (PacketMsg payloadType body)
