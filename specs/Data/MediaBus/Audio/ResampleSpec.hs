module Data.MediaBus.Audio.ResampleSpec ( spec ) where

import           Data.MediaBus
import           Data.MediaBus.Internal.Series
import           Conduit
import           Test.QuickCheck
import           Test.Hspec
import           Control.Monad.State.Strict
import           Data.Default

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

resampleAndConsume :: IsAudioSample a
                   => Source Identity (Stream' 8000 (SampleBuffer a))
                   -> a
                   -> SampleBuffer a
resampleAndConsume vvv lastVal =
    runConduitPure (vvv .|
                        resample8to16kHz' lastVal .|
                        concatStreamContents)

singleFrameFromList :: Monad m
                    => [S16]
                    -> Source m (Stream' 8000 (SampleBuffer S16))
singleFrameFromList x = mapOutput (MkStream . Next)
                                  (mapOutput (MkFrame () def)
                                             (yield (sampleBufferFromList x)) .|
                                       deriveFrameTimestamp 0)

framesFromLists :: Monad m
                => [[S16]]
                -> Source m (Stream' 8000 (SampleBuffer S16))
framesFromLists xs = mapOutput (MkStream . Next)
                               (mapOutput (MkFrame () def)
                                          (mapM_ (yield . sampleBufferFromList)
                                                 xs) .|
                                    deriveFrameTimestamp 0)
