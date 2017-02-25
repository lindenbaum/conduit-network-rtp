{-# LANGUAGE UndecidableInstances #-}

module Data.MediaBus.Packetizer
    ( CanSplitAfterDuration(..)
    , repacketizeC
    ) where

import           Conduit
import           Data.MediaBus.Stream
import           Data.MediaBus.Clock
import           Data.MediaBus.Series
import           Data.Time.Clock
import           Data.Default
import           Control.Monad.State.Strict
import           GHC.TypeLits
import           Control.Lens

-- | The packetizer splits large packets into smaller pieces of the given size,
-- if the size is not divisible by the packet size, the last packet might be
-- smaller. The sequence numbers will be offsetted by the number extra frames
-- generated.
repacketizeC :: (Num s, Monad m, HasDuration c, CanSplitAfterDuration c, Default i, KnownNat r, Integral t)
             => NominalDiffTime
             -> Conduit (Stream i s (Ticks r t) c) m (Stream i s (Ticks r t) c)
repacketizeC !packetDuration =
    evalStateC 0 $ awaitForever go
  where
    go (MkStream (Next (MkFrame !t !s !cIn))) =
        yieldLoop cIn 0
      where
        yieldLoop !c !timeOffset =
            case splitAfterDuration packetDuration c of
                Just (!packet, !rest) -> do
                    yieldWithAdaptedSeqNumAndTimestamp packet
                    modify (+ 1)
                    let packetDurationInTicks = nominalDiffTime #
                            getDuration packet
                    yieldLoop rest (timeOffset + packetDurationInTicks)
                Nothing -> yieldWithAdaptedSeqNumAndTimestamp c
          where
            yieldWithAdaptedSeqNumAndTimestamp !p = do
                !seqNumOffset <- get
                yieldNextFrame (MkFrame (t + timeOffset) (s + seqNumOffset) p)
    go (MkStream (Start !frmCtx)) =
        yieldStartFrameCtx frmCtx

-- | Class of types that support splitting of from the front a packet containing
-- roughly a certain duration.
class CanSplitAfterDuration a where
    -- | Try to split the packet into the a part which has at most the given
    -- duration and a rest. If not possible, e.g. because the input data is
    -- already shorter than the given duration, return `Nothing`.
    splitAfterDuration :: NominalDiffTime -> a -> Maybe (a, a)-- TODO make the repacketization create ONLY valid sized packets, even if that means dropping content
                                                              -- TODO allow repacketization to combine the packets
