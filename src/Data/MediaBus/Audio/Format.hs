{-# LANGUAGE TemplateHaskell #-}

-- | Audio formats
module Data.MediaBus.Audio.Format
    ( ChannelLayout(..)
    , HasChannelLayout(..)
    , SampleType(..)
    , HasSampleType(..)
    , Format(..)
    , formatSampleType
    , formatClockRate
    , formatChannelLayout
    , FormatProxy(..)
    ) where

import           Data.MediaBus.Buffer
import           Data.MediaBus.Clock
import           Data.Proxy
import           Data.Kind
import           Data.Int
import           Data.Word
import           Control.Lens
import           Foreign.Storable
import           Data.Bits

data ChannelLayout = SingleChannel | ChannelPair
    deriving (Show, Eq, Ord, Enum)

makeClassy ''ChannelLayout

data family MultiChannelSampleType (t :: ChannelLayout) s

newtype instance
        MultiChannelSampleType 'SingleChannel s = SingleChannelSample s
                                                deriving (Storable, Eq, Ord)

data instance
     MultiChannelSampleType 'ChannelPair
       s = ChannelPairSample{leftSample :: s, rightSample :: s}
         deriving (Eq, Ord)

instance Storable (MultiChannelSampleType 'ChannelPair Word8) where
    sizeOf _ = 16
    alignment (ChannelPairSample l _r) =
        alignment l
    peekByteOff ptr off = do
        (x :: Word16) <- peekByteOff ptr off
        let l = fromIntegral (x .&. 0xFF)
            r = fromIntegral (x `shiftR` 8)
        return (ChannelPairSample l r)
    pokeByteOff ptr off (ChannelPairSample l r) = do
        let x = (fromIntegral r `shiftR` 8) .|. fromIntegral l
            x :: Word16
        pokeByteOff ptr off x

instance Storable (MultiChannelSampleType 'ChannelPair Int16) where
    sizeOf _ = 32
    alignment (ChannelPairSample l _r) =
        alignment l
    peekByteOff ptr off = do
        (x :: Word32) <- peekByteOff ptr off
        let l = fromIntegral (x .&. 0xFFFF)
            r = fromIntegral (x `shiftR` 16)
        return (ChannelPairSample l r)
    pokeByteOff ptr off (ChannelPairSample l r) = do
        let x = (fromIntegral r `shiftR` 16) .|. fromIntegral l
            x :: Word32
        pokeByteOff ptr off x

instance Storable s =>
         Storable (MultiChannelSampleType 'ChannelPair s) where
    sizeOf s = 2 * sizeOf s
    alignment = alignment
    peekByteOff ptr off = do
        l <- peekByteOff ptr off
        let rOffset = sizeOf l
        r <- peekByteOff ptr (rOffset + off)
        return (ChannelPairSample l r)
    pokeByteOff ptr off (ChannelPairSample l r) = do
        pokeByteOff ptr off l
        let rOffset = sizeOf l
        pokeByteOff ptr (off + rOffset) r

instance HasChannelLayout (Proxy 'SingleChannel) where
    channelLayout = lens (const SingleChannel) const

instance HasChannelLayout (Proxy 'ChannelPair) where
    channelLayout = lens (const ChannelPair) const

data SampleType = ALaw | ULaw | Signed16Bit
    deriving (Show, Eq, Ord, Enum)

makeFields ''SampleType

type family AudioSampleType (t :: SampleType) where
        AudioSampleType 'ALaw = Word8
        AudioSampleType 'ULaw = Word8
        AudioSampleType 'Signed16Bit = Int16

class HasSampleType s where
    sampleType :: Lens' s SampleType

instance HasSampleType (Proxy 'ALaw) where
    sampleType = lens (const ALaw) const

instance HasSampleType (Proxy 'ULaw) where
    sampleType = lens (const ULaw) const

instance HasSampleType (Proxy 'Signed16Bit) where
    sampleType = lens (const Signed16Bit) const

data Format = MkFormat { _formatSampleType    :: SampleType
                       , _formatClockRate     :: UtcClock
                       , _formatChannelLayout :: ChannelLayout
                       }
    deriving (Eq, Show)

makeLenses ''Format

data FormatProxy ::
     SampleType -> ClockRate -> ChannelLayout -> Type where
        MkFormatProxy :: FormatProxy f c l

instance (HasSampleType (Proxy f), HasClock (Proxy r), GetClock (Proxy r) ~ UtcClock, HasChannelLayout (Proxy c)) =>
         HasSampleFormat (FormatProxy f r c) where
    type GetSampleFormat (FormatProxy f r c) = Format
    type SetSampleFormat (FormatProxy f r c) t = FormatProxy f r c
    type ToSampleType (FormatProxy f r c) = MultiChannelSampleType c (AudioSampleType f)
    sampleFormat f px = px <$ f fromPx
      where
        fromPx = MkFormat (fProxy px ^. sampleType)
                          (rProxy px ^. clock)
                          (cProxy px ^. channelLayout)
        fProxy :: FormatProxy f r c -> Proxy f
        fProxy _ = Proxy
        rProxy :: FormatProxy f r c -> Proxy r
        rProxy _ = Proxy
        cProxy :: FormatProxy f r c -> Proxy c
        cProxy _ = Proxy

instance HasChannelLayout (Proxy c) =>
         HasChannelLayout (FormatProxy f r c) where
    channelLayout f px = px <$ f (chProxy px ^. channelLayout)
      where
        chProxy :: FormatProxy f r c -> Proxy c
        chProxy _ = Proxy

instance (GetClock (Proxy r) ~ UtcClock, HasClock (Proxy r)) =>
         HasClock (FormatProxy f r c) where
    type GetClock (FormatProxy f r c) = UtcClock
    type SetClock (FormatProxy f r c) t = (FormatProxy f r c)
    clock f px = px <$ f (rProxy px ^. clock)
      where
        rProxy :: FormatProxy f r c -> Proxy r
        rProxy _ = Proxy
