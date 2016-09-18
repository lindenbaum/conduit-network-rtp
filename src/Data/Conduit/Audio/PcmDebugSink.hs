module Data.Conduit.Audio.PcmDebugSink
  ( debugPlayback16kMono
  , debugPlayback8kMono ) where

import Data.Conduit.Audio.Pcm
import System.IO (Handle, hClose)
import System.Process (shell)
import Data.Streaming.Process ( streamingProcess
                              , Inherited(..)
                              , waitForStreamingProcess)
import qualified Data.ByteString as B
import Data.Vector.Storable.ByteString (vectorToByteString)
import           Data.Conduit
import           Control.Monad.IO.Class

-- | Playback to sound card using /sox/
debugPlayback16kMono
  :: MonadIO m => Sink Pcm16KMono m ()
debugPlayback16kMono = do
  let cp = shell "play -r 16000 -b 16 -c1  -e signed-integer -t raw -"
  (!(sinH :: Handle), Inherited, Inherited, cph) <- streamingProcess cp
  awaitForever (pcmToByteString sinH)
  liftIO (hClose sinH)
  _ <- waitForStreamingProcess cph
  return ()
  where
    pcmToByteString !h (Pcm !d) =
      liftIO (B.hPut h (vectorToByteString d))

-- | Playback to sound card using /sox/
debugPlayback8kMono
  :: MonadIO m => Sink Pcm8KMono m ()
debugPlayback8kMono = do
  let cp = shell "play -r 8000 -b 16 -c1  -e signed-integer -t raw -"
  (!(sinH :: Handle), Inherited, Inherited, cph) <- streamingProcess cp
  awaitForever (pcmToByteString sinH)
  liftIO (hClose sinH)
  _ <- waitForStreamingProcess cph
  return ()
  where
    pcmToByteString !h (Pcm !d) =
      liftIO (B.hPut h (vectorToByteString d))
