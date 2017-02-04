module Data.MediaBus.Audio.ResampleSpec ( spec ) where

import           Data.MediaBus
import           Conduit
import           Test.QuickCheck
import           Test.Hspec
import           Data.Word
import           Control.Monad.State.Strict
import           Control.Lens
import           Data.Typeable

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

resampleAndConsume :: (Typeable s, Show w, Integral w, IsAudioSample s)
                   => FrameSource () (SampleBuffer s) (Timing 8000 w) Identity
                   -> s
                   -> SampleBuffer s
resampleAndConsume vvv lastVal =
    runConduitPure (runFrameC (vvv `connectFrameC`
                                   dbgShowFrameC "before" `connectFrameC`
                                   resample8to16kHz lastVal `connectFrameC`
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

concatFrameBuffers :: (HasSampleBuffer a, Monad m)
                   => FrameSink a t (GetSampleBuffer a) m
concatFrameBuffers = MkFrameC (loop mempty)
  where
    loop x = await >>= maybe (return x) (loop . mappend x . view sampleBuffer)


instance HasDuration (SampleBuffer S16) where
    getIntegralDuration = fromIntegral . sampleCount
