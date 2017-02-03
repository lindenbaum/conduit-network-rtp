module Data.MediaBus.Audio.Resample ( resample8to16kHz ) where

import           Foreign.Storable
import           Data.MediaBus.Frame
import           Data.MediaBus.Clock
import           Data.MediaBus.Sample
import           Conduit
import           Data.Bits
import qualified Data.Vector.Generic          as G
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as M
import           Control.Monad.State.Strict
import           Control.Lens
import           Data.List                    as L
import           GHC.TypeLits

resample8to16kHz :: forall s c m r r2.
                 (Storable s, Num s, Bits s, Monad m, GetClockRate c ~ r, KnownNat r, r2 ~ (r + r), KnownNat r2, IsTiming c, IsTiming (SetClockRate c r2), Num (Ticks (SetClockRate c r2)))
                 => s
                 -> FrameBufferFilter s s c (SetClockRate c r2) m
resample8to16kHz sInitial =
    MkFrameC (evalStateC sInitial (runFrameC (frameBufferFilterM resample)))
        `connectFrameC` adaptClock
  where
    adaptClock = frameFilter (over frame adaptTimingSync)

    resample sb = do
        lastVal <- get
        put (V.last (sb ^. sampleVector))
        return (createSampleBufferFrom (interpolate lastVal) sb)
      where
        interpolate !lastVal !vIn = do
            let lenOut = 2 * V.length vIn
            vOut <- M.new lenOut
            void $ G.imapM (\i s -> M.write vOut (i * 2 + 1) s) vIn
            void $
                L.foldl' (interpolateSamples vOut)
                         (return lastVal)
                         [0 .. lenOut - 1]
            return vOut
          where
            interpolateSamples !vOut !mPrev !i =
                if even i
                then do
                    !prev <- mPrev
                    !next <- M.unsafeRead vOut (i + 1)
                    M.unsafeWrite vOut
                                  i
                                  ((next `unsafeShiftR` 1) +
                                       (prev `unsafeShiftR` 1))
                    mPrev
                else M.unsafeRead vOut i
