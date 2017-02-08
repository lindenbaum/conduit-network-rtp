module Data.MediaBus.Transport.Udp ( udpSource ) where

import qualified Data.ByteString              as B
import           Data.Conduit.Network.UDP
import           Data.Streaming.Network
import           Network.Socket               ( close )
import           Text.Printf
import           Data.Conduit
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Word

-- | A UDP source that uses 'MonandResource' to make sure the socket is closed.
udpSource :: MonadResource m => Int -> HostPreference -> Source m Message
udpSource !port !host = bracketP (bindPortUDP port host)
                                 close
                                 (`sourceSocket` 1024)

-- | Extract a 'B.ByteString' from a 'Message'
parseUdpMessages :: MonadIO m => Conduit Message m B.ByteString
parseUdpMessages = awaitForever (yield . msgData)
