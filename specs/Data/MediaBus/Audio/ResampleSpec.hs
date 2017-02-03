module Data.MediaBus.Audio.ResampleSpec ( spec ) where

import           Foreign.Storable
import           Data.MediaBus
import           Conduit
-- import           Test.QuickCheck
import           Test.Hspec
import           Data.Word
import           Control.Monad.State.Strict

spec :: Spec
spec = describe "Resampling of S16 samples from 8 to 16 kHz" $ do
    it "interpolates between samples" $ pending
    it "interpolates also between frames" $ pending

xxx :: Monad m
    => [S16]
    -> FrameSource () (SampleBuffer S16) (Timing 8000 Word32) m
xxx x = MkFrameC (yield (sampleBufferFromList x)) `connectFrameC`
    deriveFrameTimestamp 0

deriveFrameTimestamp :: (Storable s, Monad m, Integral (Ticks t))
                     => Reference (Ticks t)
                     -> FrameSource (SampleBuffer s) (SampleBuffer s) t m
deriveFrameTimestamp ref@(MkReference t0) =
    MkFrameC (evalStateC t0 go)
  where
    go = do
        msb <- await
        maybe (return ())
              (\sb -> do
                   modify (+ fromIntegral (sampleCount sb))
                   yield (MkFrame (SynchronizeTo ref (MkEvent t0 sb)))
                   awaitForever sendSyncedLoop)
              msb
      where
        sendSyncedLoop sb = do
            t <- get
            modify (+ fromIntegral (sampleCount sb))
            yield (MkFrame (Synchronized (MkEvent t sb)))
