module Data.MediaBus.Audio.Resample
    ( resample8to16kHz'
    , resample8to16kHz
    ) where

import           Data.MediaBus.Stream
import           Data.MediaBus.Sample
import           Data.MediaBus.Audio.Raw
import           Conduit
import qualified Data.Vector.Generic          as G
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as M
import           Control.Monad.State.Strict
import           Control.Lens
import           Data.Default
import           GHC.TypeLits

resample8to16kHz' :: (IsAudioSample sa, GetAudioSampleRate sa ~ r, IsAudioSample (SetAudioSampleRate sa (r + r)), Monad m, KnownNat (r + r), Default sa)
                  => Conduit (Stream' t (SampleBuffer sa)) m (Stream' t (SampleBuffer (SetAudioSampleRate sa (r + r))))
resample8to16kHz' = resample8to16kHz def

resample8to16kHz :: (IsAudioSample sa, GetAudioSampleRate sa ~ r, Monad m, IsAudioSample (SetAudioSampleRate sa (r + r)), KnownNat (r + r))
                 => sa
                 -> Conduit (Stream i s t (SampleBuffer sa)) m (Stream i s t (SampleBuffer (SetAudioSampleRate sa (r + r))))
resample8to16kHz sa = evalStateC sa (mapPayloadC resample)
  where
    resample sb
        | sampleCount sb == 0 = return (MkSampleBuffer mempty)
        | otherwise = do
              lastVal <- get
              put (V.last (sb ^. sampleVector))
              return (createSampleBufferFrom (interpolate lastVal) sb)
      where
        interpolate !lastVal !vIn = do
            let lenOut = 2 * V.length vIn
            vOut <- M.new lenOut
            void $
                G.imapM (\i s -> M.unsafeWrite vOut
                                               (i * 2 + 1)
                                               (doubleAudioSampleRate s))
                        vIn
            void $
                foldM (lerpSamples vOut)
                      (doubleAudioSampleRate lastVal)
                      [0 .. lenOut - 1]
            return vOut
          where
            lerpSamples !vOut !prev !i = do
                when (even i)
                     (do
                          !next <- M.unsafeRead vOut (i + 1)
                          M.unsafeWrite vOut i (avgSamples prev next))
                M.unsafeRead vOut i
