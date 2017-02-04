module Data.MediaBus.Audio.Resample ( resample8to16kHz ) where

import           Data.MediaBus.Frame
import           Data.MediaBus.Clock
import           Data.MediaBus.Sample
import           Data.MediaBus.Audio.Raw
import           Conduit
import qualified Data.Vector.Generic          as G
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as M
import           Control.Monad.State.Strict
import           Control.Lens

resample8to16kHz :: forall s m w w'.
                 (IsAudioSample s, Monad m, Num w', IsTiming (Timing 8000 w), IsTiming (Timing 16000 w'))
                 => s
                 -> FrameBufferFilter s s (Timing 8000 w) (Timing 16000 w') m
resample8to16kHz sInitial =
    MkFrameC (evalStateC sInitial (runFrameC (frameBufferFilterM resample)))
        `connectFrameC` adaptClock
  where
    adaptClock = frameFilter (over frame adaptTimingSync)

    resample sb
      | sampleCount sb == 0 = return sb
      |otherwise = do
        lastVal <- get
        put (V.last (sb ^. sampleVector))
        return (createSampleBufferFrom (interpolate lastVal) sb)
      where
        interpolate !lastVal !vIn = do
            let lenOut = 2 * V.length vIn
            vOut <- M.new lenOut
            void $ G.imapM (\i s -> M.unsafeWrite vOut (i * 2 + 1) s) vIn
            void $ foldM (lerpSamples vOut) lastVal [0 .. lenOut - 1]
            return vOut
          where
            lerpSamples !vOut !prev !i = do
                when (even i)
                     (do
                          !next <- M.unsafeRead vOut (i + 1)
                          M.unsafeWrite vOut i (avgSamples prev next))
                M.unsafeRead vOut i
