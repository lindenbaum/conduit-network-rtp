module Data.MediaBus.Transport.Udp ( udpSource ) where

import qualified Data.ByteString                             as B
import           Data.Conduit.Network.UDP                    as X
import           Data.Streaming.Network                      as X
import           Network.Socket               (close)
import           Text.Printf                                 as X
import           Data.Conduit                                as X
import           Control.Monad                               as X
import           Control.Monad.IO.Class                      as X
import           Control.Monad.Trans.Resource                as X
import           Data.Word                                   as X

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
