module Data.MediaBus.DebugSink
    ( HasDebugPlaybackSink(..)
    , streamDebugPlaybackSink
    ) where

import           Data.MediaBus.Audio.Raw
import           Data.MediaBus.Sample
import           Data.MediaBus.Stream
import           Data.MediaBus.Internal.Series
import           System.IO                     ( Handle, hClose )
import           System.Process                ( shell )
import           Data.Streaming.Process        ( Inherited(..)
                                               , streamingProcess
                                               , waitForStreamingProcess )
import qualified Data.ByteString               as B
import           Data.Conduit
import           Control.Monad.IO.Class
import           Data.Proxy
import           Text.Printf
import           GHC.TypeLits

class HasDebugPlaybackSink s t c where
    debugPlaybackSink :: MonadIO m => Sink (Frame s t c) m ()

instance KnownNat r =>
         HasDebugPlaybackSink s t (SampleBuffer (S16 r)) where
    debugPlaybackSink = do
        let cp = shell (printf "play -r %d -b 16 -c1  -e signed-integer -t raw -"
                               (natVal (Proxy :: Proxy r)))
        (!(sinH :: Handle), Inherited, Inherited, cph) <- streamingProcess cp
        awaitForever (pcmToByteString sinH)
        liftIO (hClose sinH)
        _ <- waitForStreamingProcess cph
        return ()
      where
        pcmToByteString !h (MkFrame _ _ !d) =
            liftIO (B.hPut h (byteStringFromSampleBuffer d))

streamDebugPlaybackSink :: (HasDebugPlaybackSink s t c, MonadIO m)
                        => Sink (Stream i s t c) m ()
streamDebugPlaybackSink =
    foldStreamC $ \(MkStartingFrom _) -> debugPlaybackSink
