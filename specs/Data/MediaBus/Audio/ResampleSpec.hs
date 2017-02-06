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

resampleAndConsume :: forall s w i a.
                   (Integral w, IsAudioSample a)
                   => Source Identity (FrameBuffer i s (Ticks (Timing 8000 w)) a)
                   -> a
                   -> SampleBuffer a
resampleAndConsume vvv lastVal =
    runConduitPure (vvv .|
                        (resample8to16kHz lastVal :: ConduitM (Frame i s (Ticks (Timing 8000 w)) (SampleBuffer a)) (Frame i s (Ticks (Timing 16000 Word64)) (SampleBuffer a)) Identity ()) .|
                        concatFrameContents)

singleFrameFromList :: Monad m
                    => [S16]
                    -> Source m (FrameBuffer i Int (Ticks (Timing 8000 Word32)) S16)
singleFrameFromList x = yield (sampleBufferFromList x) .|
    mapOutput (MkFrame . MkRelative . Next . MkSequenceNumbered 0 . Next)
              (deriveFrameTimestamp 0)

framesFromLists :: Monad m
                => [[S16]]
                -> Source m (FrameBuffer i Int (Ticks (Timing 8000 Word32)) S16)
framesFromLists xs = mapM_ (yield . sampleBufferFromList) xs .|
    mapOutput (MkFrame . MkRelative . Next . MkSequenceNumbered 0 . Next)
              (deriveFrameTimestamp 0)
