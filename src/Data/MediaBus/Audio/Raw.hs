{-# LANGUAGE TemplateHaskell #-}

module Data.MediaBus.Audio.Raw
    ( S16(..)
    , s16Sample
    , IsAudioSample(..)
    ) where

import           Foreign.Storable
import           Data.MediaBus.Audio.Channels
import           Data.Int
import           Control.Lens
import           Test.QuickCheck
import           Data.Bits
import           Data.Typeable
import           Data.MediaBus.Clock

newtype S16 = MkS16 { _s16Sample :: Int16 }
    deriving (Typeable, Storable, Num, Eq, Ord, Arbitrary)

instance HasDuration (Proxy S16) where
    getDuration _ = 1

instance Show S16 where
    show (MkS16 x) = show x

makeLenses ''S16

class (Show a, Storable a, Num a, Eq a, Ord a, Arbitrary a) =>
      IsAudioSample a where
    avgSamples :: a -> a -> a

instance IsAudioSample S16 where
    avgSamples (MkS16 !x) (MkS16 !y) =
        MkS16 $
            if abs x < 16382 &&
                abs y < 16382
            then (x + y) `unsafeShiftR` 1
            else (x `unsafeShiftR` 1) + (y `unsafeShiftR` 1)

instance HasChannelLayout S16 where
    channelLayout _ = SingleChannel
