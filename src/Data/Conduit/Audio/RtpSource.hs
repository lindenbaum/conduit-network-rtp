-- | Receive an RTP stream from the network using UDP,
--
-- An RTP session is maintained, containing the SSRC, sequence number and
-- relative timestamps and sequence numbers.
--
-- This is a __top-level__ module, it re-exports many other modules.
module Data.Conduit.Audio.RtpSource
  ( RtpEvent, RtpEventRaw, SessionMsg(..), SessionInfo(..), session
  , udpSession, packetDbgSink, module X
  )
where

import qualified Data.ByteString                             as B
import           Data.Conduit.Audio.RtpPacket                as X
                                              ( Packet(..)
                                              , SeqNum(..)
                                              , Header(..)
                                              , deserialize)
import           Data.Conduit                                as X
import           Data.Conduit.Audio.Event                    as X
import           Data.Conduit.Network.UDP                    as X
import           Data.Streaming.Network                      as X
import           Network.Socket               (close)
import           Text.Printf                                 as X
import           Control.Monad                               as X
import           Control.Monad.IO.Class                      as X
import           Control.Monad.Trans.Resource                as X
import           Data.Word                                   as X

-- | The in and out of band message for RTP
type RtpEvent body = Event body SessionMsg
type RtpEventRaw = Event Packet SessionMsg

-- | The type of message that are send by 'session'.
newtype SessionMsg =
   BeginSession SessionInfo
  deriving Show

data SessionInfo =
  SessionInfo { siSsrc :: !Word32
              , siStartSeq :: !SeqNum
              , siStartTimeStamp :: !Word32 }
  deriving Show

-- | Now something interesting. A monadic bind of consumers will sequence the
-- conduits. We start a new RTP Session by first waiting for the first packet,
-- then pass control over to a conduit that knows the SSRC and exits when
-- another SSRC comes. The first packet also sends events for the 'JitterBuffer'
-- and the audio decoder.
session
  :: MonadIO m
  => Conduit Packet m RtpEventRaw
session = (initiateSession >>= mapM_ continueSession >> session)

-- | Wait for the first RTP packet and send 'Beginsession'.
initiateSession
  :: Monad m
  => ConduitM Packet RtpEventRaw m (Maybe SessionInfo)
initiateSession = do
  mp <- await
  case mp of
    Nothing ->
      return Nothing
    Just p@(Packet Header{..} _) ->
      do let !si = SessionInfo ssrc sequenceNumber timestamp
         yieldOutOfBand (BeginSession si)
         leftover p
         return (Just si)

-- | Convert 'Packet' messages to 'RtpEvent's until the ssrc changes, in that case
-- send a 'EndSession' 'OutOfBand'. TODO Add RTCP support, RTCP defines /BYE/ messages.
continueSession
  :: Monad m
  => SessionInfo -> Conduit Packet m RtpEventRaw
continueSession SessionInfo{..} = await >>= mapM_ go
  where
    go !p@(Packet Header{..} _) =
      when (ssrc == siSsrc) (yieldInband sequenceNumber p)

-- | Listen to incoming RTP packets on a UDP port.
udpSession
  :: MonadResource m
  => Int
  -> HostPreference
  -> Source m RtpEventRaw
udpSession !port !host =
  udpSource port host =$= parseUdpMessages =$= parsePackets =$= session

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

-- | Internal debug helper
packetDbgSink
  :: MonadIO m
  => Sink Packet m ()
packetDbgSink =
  awaitForever $
  \ (Packet Header{..} _) ->
    liftIO $ do
      when hasMarker (putStrLn "         !!!")
      printf "SSRC: %0.4x | TS: %0.4x | SEQ: %d\n" ssrc timestamp (unSeqNum sequenceNumber)
