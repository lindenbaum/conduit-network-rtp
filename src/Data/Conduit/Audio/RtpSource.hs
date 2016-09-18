-- | Receive an RTP stream from the network using UDP,
--
-- An RTP session is maintained, containing the SSRC, sequence number and
-- relative timestamps and sequence numbers.
module Data.Conduit.Audio.RtpSource ( ) where

import Data.Conduit.Audio.RtpPacket (Packet(..), Header(..), deserialize)
import qualified Data.ByteString as B
import           Data.Conduit
import           Data.Conduit.Network.UDP
import           Data.Streaming.Network
import           Network.Socket               (close)
import           Text.Printf
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource

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
  => Conduit B.ByteString m (Packet B.ByteString)
parsePackets =
  awaitForever (yield . deserialize)

packetDbgSink
  :: MonadIO m
  => Sink (Packet B.ByteString) m ()
packetDbgSink =
  awaitForever $
  \ (Packet Header{..} _) ->
    liftIO $ do
      when hasMarker (putStrLn "         !!!")
      printf "SSRC: %0.4x | TS: %0.4x | SEQ: %d\n" ssrc timestamp sequenceNumber

-- | The in and out of band message for RTP
type RtpEvent = Event PacketMsg SessionMsg

-- | A new packet to process
newtype PacketMsg = PacketMsg !B.ByteString

-- | The type of message that are send by 'session'.
data SessionMsg =
   BeginSession !SessionInfo
 | EndSession

data SessionInfo =
  SessionInfo { siSsrc :: !Word32
              , siStartSeq :: !SeqNum
              , siStartTimeStamp :: !SeqNum
              , siPayloadType :: !Word8
              }

-- | Now something interesting. A monadic bind of consumers will sequence the
-- conduits. We start a new RTP Session by first waiting for the first packet,
-- then pass control over to a conduit that knows the SSRC and exits when
-- another SSRC comes. The first packet also sends events for the 'JitterBuffer'
-- and the audio decoder.
session
  :: MonadIO m
  => Conduit (Packet B.ByteString) m RtpEvent
session = initiateSession >>= continueSession >> session

-- | Wait for the first RTP packet and send 'Beginsession'.
initiateSession
  :: Monad m
  => ConduitM (Packet B.ByteString) RtpEvent m (Maybe SessionInfo)
initiateSession = do
  mp <- await
  case mp if
    Nothing -> return Nothing
    Just (Packet Header{..} !body) ->
      do let !si = SessionInfo ssrc sequenceNumber timestamp payloadType
         yieldOutOfBand (BeginSession si)
         yieldInband sequenceNumber (PacketMsg body)
