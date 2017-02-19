{-# LANGUAGE UndecidableInstances #-}

module Data.MediaBus.Packetizer
    ( CanSplitAfterDuration(..)
    , repacketizeC
    ) where

import           Conduit
import           Data.MediaBus.Stream
import           Data.Time.Clock
import           Data.Default
import           Control.Monad.State.Strict

-- | The packetizer splits large packets into smaller pieces of the given size,
-- if the size is not divisible by the packet size, the last packet might be
-- smaller. The sequence numbers will be offsetted by the number extra frames
-- generated.
repacketizeC :: (Num s, Monad m, CanSplitAfterDuration c, Default i)
             => NominalDiffTime
             -> Conduit (Stream i s t c) m (Stream i s t c)
repacketizeC packetDuration =
    overFramesC go
  where
    go _ = evalStateC 0 (awaitForever handleFrames)
    handleFrames (MkFrame t s cIn) =
        yieldLoop cIn
      where
        yieldLoop c = case splitAfterDuration packetDuration c of
            Just (packet, rest) -> do
                yieldWithAdaptedSeqNum packet
                modify (+ 1)
                yieldLoop rest
            Nothing -> yieldWithAdaptedSeqNum c
          where
            yieldWithAdaptedSeqNum p = do
                seqNumOffset <- get
                yield (MkFrame t (s + seqNumOffset) p)

-- | Class of types that support splitting of from the front a packet containing
-- roughly a certain duration.
class CanSplitAfterDuration a where
    -- | Try to split the packet into the a part which has at most the given
    -- duration and a rest. If not possible, e.g. because the input data is
    -- already shorter than the given duration, return `Nothing`.
    splitAfterDuration :: NominalDiffTime -> a -> Maybe (a, a)
