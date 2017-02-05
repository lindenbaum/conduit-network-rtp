module Data.MediaBus.Audio.ResampleSpec ( spec ) where

import           Data.MediaBus
import           Conduit
import           Test.QuickCheck
import           Test.Hspec
import           Data.Word
import           Control.Monad.State.Strict

spec :: Spec
spec = describe "Resampling of S16 samples from 8 to 16 kHz" $ do
    it "interpolates between samples" $
        let lastVal = 0
        in
            property $
                \samples -> sampleBufferToList (resampleAndConsume (singleFrameFromList samples)
                                                                   lastVal)
                    `shouldBe` expectedResamplingResult samples lastVal
    it "interpolates also between frames" $
        let lastVal = 0
        in
            property $
                \samplesLists -> sampleBufferToList (resampleAndConsume (framesFromLists samplesLists)
                                                                        lastVal)
                    `shouldBe` expectedResamplingResult (join samplesLists)
                                                        lastVal

expectedResamplingResult :: [S16] -> S16 -> [S16]
expectedResamplingResult xs lastVal =
    concatMap (\(x, y) -> [ avgSamples x y, y ]) (zip (lastVal : xs) xs)

resampleAndConsume :: forall s w.
                   (Integral w, IsAudioSample s)
                   => FrameSource () (SampleBuffer s) (Timing 8000 w) Identity
                   -> s
                   -> SampleBuffer s
resampleAndConsume vvv lastVal =
    runConduitPure (runFrameC (vvv `connectFrameC`
                                   (resample8to16kHz lastVal :: FrameBufferFilter s s (Timing 8000 w) (Timing 16000 w) Identity) `connectFrameC`
                                   concatFrameBuffers))

singleFrameFromList :: Monad m
                    => [S16]
                    -> FrameSource () (SampleBuffer S16) (Timing 8000 Word32) m
singleFrameFromList x = MkFrameC (yield (sampleBufferFromList x) .|
                                      mapOutput MkFrame (deriveFrameTimestamp 0))

framesFromLists :: Monad m
                => [[S16]]
                -> FrameSource () (SampleBuffer S16) (Timing 8000 Word32) m
framesFromLists xs = MkFrameC (mapM_ (yield . sampleBufferFromList) xs .|
                                   mapOutput MkFrame (deriveFrameTimestamp 0))
