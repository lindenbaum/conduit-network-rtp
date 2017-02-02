module Data.MediaBus.Audio.Channels
    ( ChannelLayout(..)
    , ChannelPair(..)
    , leftSample
    , rightSample
    , HasChannelLayout(..)
    ) where

import           Data.MediaBus.Sample
import           Control.Lens
import           Foreign.Storable

data ChannelLayout = SingleChannel | ChannelPair
    deriving (Show, Eq, Ord, Enum)

class HasChannelLayout c where
    channelLayout :: c -> ChannelLayout

data ChannelPair a = MkChannelPair { _leftSample  :: a
                                   , _rightSample :: a
                                   }
    deriving (Show, Eq, Ord)

makeLenses ''ChannelPair

instance HasChannelLayout a =>
         HasChannelLayout (ChannelPair a) where
    channelLayout MkChannelPair{_leftSample,_rightSample} =
        case (channelLayout _leftSample, channelLayout _rightSample) of
            (SingleChannel, SingleChannel) ->
                ChannelPair
            other -> error ("Sorry this channel layout is not supported: " ++
                                show other)

instance Storable s =>
         Storable (ChannelPair s) where
    sizeOf s = 2 * sizeOf s
    alignment = alignment
    peekByteOff ptr off = do
        l <- peekByteOff ptr off
        let rOffset = sizeOf l
        r <- peekByteOff ptr (rOffset + off)
        return (MkChannelPair l r)
    pokeByteOff ptr off (MkChannelPair l r) = do
        pokeByteOff ptr off l
        let rOffset = sizeOf l
        pokeByteOff ptr (off + rOffset) r
