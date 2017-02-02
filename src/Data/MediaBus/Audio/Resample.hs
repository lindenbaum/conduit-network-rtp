module Data.MediaBus.Audio.Resample (resample8to16kHz) where


import           Data.MediaBus.Frame
import           Data.MediaBus.Sample
import           Conduit
import           Data.Bits
import qualified Data.Vector.Generic          as G
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as M
import           Control.Monad.State.Strict
import           Control.Lens
import           Data.List                    as L

resample8to16kHz :: forall s c m.
                 (Storable s, Num s, Bits s, Monad m)
                 => s
                 -> FrameBufferFilter s s c c m
resample8to16kHz sInitial =
    MkFrameC (evalStateC sInitial (runFrameC (frameBufferFilterM resample)))
  where
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
