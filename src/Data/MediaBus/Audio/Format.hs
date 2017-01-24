{-# LANGUAGE TemplateHaskell #-}

module Data.MediaBus.Audio.Format ( ) where

import           Data.MediaBus.Clock
import           Data.Proxy
import           Data.Kind
import           Control.Lens

data ChannelLayout = SingleChannel | ChannelPair
    deriving (Show, Eq, Ord, Enum)

makeClassy ''ChannelLayout

instance HasChannelLayout (Proxy 'SingleChannel) where
    channelLayout = lens (const SingleChannel) const

instance HasChannelLayout (Proxy 'ChannelPair) where
    channelLayout = lens (const ChannelPair) const

data SampleType = ALaw | ULaw | Signed16Bit
    deriving (Show, Eq, Ord, Enum)

makeClassy ''SampleType

instance HasSampleType (Proxy 'ALaw) where
    sampleType = lens (const ALaw) const

instance HasSampleType (Proxy 'ULaw) where
    sampleType = lens (const ULaw) const

instance HasSampleType (Proxy 'Signed16Bit) where
    sampleType = lens (const Signed16Bit) const

data Format = MkFormat { _formatSampleType    :: SampleType
                       , _formatClockRate     :: Clock
                       , _formatChannelLayout :: ChannelLayout
                       }
    deriving (Eq, Show)

makeClassy ''Format

data FormatProxy ::
     SampleType -> ClockRate -> ChannelLayout -> Type where
        MkFormatProxy :: FormatProxy f c l

instance (HasClock (Proxy c), HasSampleType (Proxy f), HasChannelLayout (Proxy l)) =>
         HasFormat (FormatProxy f c l) where
    format f px = undefined

instance HasChannelLayout (Proxy c) =>
         HasChannelLayout (FormatProxy f r c) where
    channelLayout = undefined
      where
        chProxy :: FormatProxy f r c -> Proxy c
        chProxy _ = Proxy

instance HasClock (Proxy r) =>
         HasClock (FormatProxy f r c) where
    clock = undefined
      where
        rProxy :: FormatProxy f r c -> Proxy r
        rProxy _ = Proxy
