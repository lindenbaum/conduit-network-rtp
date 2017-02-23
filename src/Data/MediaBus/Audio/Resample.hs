module Data.MediaBus.Audio.Resample ( resample8to16kHz' ) where

import           Data.MediaBus.Stream
import           Data.MediaBus.Sample
import           Data.MediaBus.Audio.Raw
import           Conduit
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as M
import           Control.Monad.State.Strict
import           Control.Lens
import           Control.Parallel.Strategies  ( NFData )

resample8to16kHz' :: (NFData s, NFData t, NFData i, IsAudioSample sa, GetAudioSampleRate sa ~ 8000, Monad m, IsAudioSample (SetAudioSampleRate sa 16000), NFData (SetAudioSampleRate sa 16000))
                  => sa
                  -> Conduit (Stream i s t (SampleBuffer sa)) m (Stream i s t (SampleBuffer (SetAudioSampleRate sa 16000)))
resample8to16kHz' !sa = evalStateC sa (mapPayloadC' resample)
  where
    resample !sb
        | sampleCount sb == 0 = return (MkSampleBuffer mempty)
        | otherwise = do
              !lastVal <- get
              put (V.last (sb ^. sampleVector))
              return (createSampleBufferFrom (interpolate lastVal) sb)
      where
        interpolate !lastVal !vIn = do
            let !lenOut = 2 * lenIn
            !vOut <- M.new lenOut
            void $ lerpSamples vOut (doubleAudioSampleRate lastVal) 0
            return vOut
          where
            !lenIn = V.length vIn
            lerpSamples !vOut !prev !i =
                when (i < lenIn) $ do
                    !next <- doubleAudioSampleRate <$> V.indexM vIn i
                    M.unsafeWrite vOut (2 * i) (avgSamples prev next)
                    M.unsafeWrite vOut (2 * i + 1) next
                    lerpSamples vOut next (i + 1)
