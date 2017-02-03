{-# LANGUAGE TemplateHaskell #-}

module Data.MediaBus.Audio.Raw
    ( S16(..)
    , s16Sample
    ) where

import           Foreign.Storable
import           Data.MediaBus.Audio.Channels
import           Data.Int
import           Data.Bits
import           Control.Lens

newtype S16 = MkS16 { _s16Sample :: Int16 }
    deriving (Show, Storable, Num, Eq, Ord, Bits)

makeLenses ''S16

instance HasChannelLayout S16 where
    channelLayout _ = SingleChannel
