{-# LANGUAGE TemplateHaskell #-}

-- | Audio formats
module Data.MediaBus.Audio.Raw
    ( ChannelLayout(..)
    , ChannelPair(..)
    , leftSample
    , rightSample
    , HasChannelLayout(..)
    , ALaw(..)
    , alawSample
    , ULaw(..)
    , ulawSample
    , S16(..)
    , s16Sample
    ) where

import           Data.MediaBus.Sample
import           Data.Int
import           Data.Word
import           Control.Lens
import           Foreign.Storable

data ChannelLayout = SingleChannel
                   | ChannelPair
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

newtype ALaw = MkALaw {_alawSample :: Word8}
    deriving (Show, Storable)

makeLenses ''ALaw

instance HasChannelLayout ALaw where
    channelLayout _ = SingleChannel

newtype ULaw = MkULaw { _ulawSample :: Word8 }
    deriving (Show, Storable)

makeLenses ''ULaw

instance HasChannelLayout ULaw where
    channelLayout _ = SingleChannel

newtype S16 = MkS16 {s16Sample :: Int16}
    deriving (Show, Storable, Num, Eq, Ord)

makeLenses ''S16

instance HasChannelLayout S16 where
    channelLayout _ = SingleChannel
