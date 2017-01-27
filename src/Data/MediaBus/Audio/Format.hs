{-# LANGUAGE TemplateHaskell #-}

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

import           Data.MediaBus.Basics
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

makeFields ''SampleType

class HasSampleType s where
    sampleType :: Lens' s SampleType

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

makeLenses ''Format

data FormatProxy ::
     SampleType -> ClockRate -> ChannelLayout -> Type where
        MkFormatProxy :: FormatProxy f c l

instance (HasSampleType (Proxy f), HasClock (Proxy r), GetClock (Proxy r) ~ Clock, HasChannelLayout (Proxy c)) =>
         HasFormat (FormatProxy f r c) where
    type GetFormat (FormatProxy f r c) = Format
    type SetFormat (FormatProxy f r c) t = (FormatProxy f r c)
    format f px = px <$ f fromPx
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

instance (GetClock (Proxy r) ~ Clock, HasClock (Proxy r)) =>
         HasClock (FormatProxy f r c) where
    type GetClock (FormatProxy f r c) = Clock
    type SetClock (FormatProxy f r c) t = (FormatProxy f r c)
    clock f px = px <$ f (rProxy px ^. clock)
      where
        rProxy :: FormatProxy f r c -> Proxy r
        rProxy _ = Proxy
