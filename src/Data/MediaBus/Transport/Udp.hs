module Data.MediaBus.Transport.Udp ( udpDatagramSource ) where

import           Control.Lens
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Resource
import           Conduit
import           Data.Conduit.Network.UDP
import           Data.MediaBus.Clock
import           Data.MediaBus.Stream
import           Data.MediaBus.SourceId
import           Data.MediaBus.Sequence
import           Data.MediaBus.Internal.Series
import           Data.MediaBus.Internal.Conduit ( dbgShowSink )
import           Data.Streaming.Network
import           Network.Socket                 ( SockAddr, close )
import qualified Data.ByteString                as B
import           Data.Word

-- | A UDP source that uses 'MonandResource' to make sure the socket is closed.
udpDatagramSource :: (IsClock c, MonadClock c m, MonadResource m, Num s)
                  => proxy c
                  -> Int
                  -> HostPreference
                  -> Source m (Stream (SourceId SockAddr) (SeqNum s) (TimeDiff c) B.ByteString)
udpDatagramSource _clk port host = do
    t0 <- lift now
    bracketP (bindPortUDP port host) close (`sourceSocket` 1024) .|
        evalStateC (Nothing, 0, t0) (awaitForever createFrame)
  where
    createFrame m      -- TODO use foldSeriesC here
     = do
        let currentSender = msgSender m
        lastSender <- _1 <<.= Just currentSender
        tNow <- lift (lift now)
        when (Just currentSender /= lastSender) $ do
            _2 .= 0
            _3 .= tNow
            yield (MkStream (Start (MkFrameCtx (MkSourceId currentSender)
                                               (timeAsTimeDiff tNow)
                                               0)))
        sn <- _2 <<+= 1
        tStart <- use _3
        yield (MkStream (Next (MkFrame (diffTime tNow tStart) sn (msgData m))))

_receiveFromUDP :: IO [(Stream (SourceId SockAddr) (SeqNum Word64) (TimeDiff UtcClock) B.ByteString)]
_receiveFromUDP = runConduitRes (udpDatagramSource useUtcClock 10000 "127.0.0.1" .|
                                     --    rtpSource .|
                                     dbgShowSink 1
                                                 "RTP")
