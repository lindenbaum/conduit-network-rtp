{-# LANGUAGE BangPatterns #-}
module Data.Conduit.Audio.Event
  (
  -- * Basic Event Conduits
    type Event
  , yieldInbandGap
  , yieldInband
  , yieldOutOfBand
  , awaitEventForever
  , InOrOutOfBand(..)
  , SequenceOf(..)
  , HasGaps(..)
  )
where

import           Data.Conduit
import           Data.Conduit.Audio.RtpPacket (SeqNum (..))

-- | Events
type Event a b = InOrOutOfBand (SequenceOf (HasGaps a)) b

yieldInbandGap :: Monad m => SeqNum -> Producer m (Event a b)
yieldInbandGap !s = yield (InBand (SequenceOf s Gap))

yieldInband :: Monad m => SeqNum -> a -> Producer m (Event a b)
yieldInband !s !a = yield (InBand (SequenceOf s (NoGap a)))

yieldOutOfBand :: Monad m => b -> Producer m (Event a b)
yieldOutOfBand !b = yield (OutOfBand b)

awaitEventForever
  :: Monad m
  => (SeqNum -> a -> Producer m c) -- ^ callback for inband payload
  -> (SeqNum -> Producer m c)  -- ^ callback for inband gaps
  -> (b -> Producer m c) -- ^ callback for out of band data
  -> Conduit (Event a b) m c
awaitEventForever onInband onInbandGap onOob = awaitForever go
  where
    go (InBand (SequenceOf !s (NoGap !a))) = onInband s a
    go (InBand (SequenceOf !s Gap)) = onInbandGap s
    go (OutOfBand !b) = onOob b

-- | Wraps events and allows "out-ouf-band" events that have
-- no sequence number.
data InOrOutOfBand a b = InBand !a | OutOfBand !b
  deriving Show

-- | Something that happend before or after something else.
data SequenceOf a =
    SequenceOf { position :: !SeqNum
               , payload :: !a }
  deriving Show

instance Eq (SequenceOf a) where
  (SequenceOf !l _) == (SequenceOf !r _) = l == r

instance Ord (SequenceOf a) where
  (SequenceOf !l _) <= (SequenceOf !r _) = l <= r

-- | A wrapper for cases where the signal is interrupted.
data HasGaps a = NoGap !a | Gap
  deriving Show
