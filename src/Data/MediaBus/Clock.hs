module Data.MediaBus.Clock
    ( tickDiffTime
    , timeOf
    , Clock(..)
    , clockTick
    , HasClock(..)
    , HasTimestamp(..)
    , ClockRate(..)
    , At8kHz
    , At12kHz
    , At16kHz
    , At22050Hz
    , At32kHz
    , At44100Hz
    , At48kHz
    ) where

import           Data.Time.Clock
import           GHC.TypeLits
import           Data.Proxy
import           Control.Lens

newtype Clock = MkClock { _clockTick :: NominalDiffTime }
    deriving (Show, Eq, Ord)

makeLenses ''Clock

class SetClock s (GetClock s) ~ s =>
      HasClock s where
    type GetClock s
    type SetClock s t
    clock :: Lens s (SetClock s t) (GetClock s) t

instance HasClock Clock where
    type GetClock Clock = Clock
    type SetClock Clock t = t
    clock = iso id id

-- | A type class for things that have a time stamp
class SetTimestamp s (GetTimestamp s) ~ s =>
      HasTimestamp s where
    type SetTimestamp s t
    type GetTimestamp s
    timestamp :: Lens s (SetTimestamp s t) (GetTimestamp s) t

-- | A type class for things that are a clock
class IsClock t where
    type ReferenceTime t
    type Timestamp t

-- | Return the number of timestamp that a temopral value represents
timeOf :: Integral t => Clock -> t -> NominalDiffTime
timeOf c t = fromInteger (toInteger t) * _clockTick c

tickDiffTime :: (Integral t) => Clock -> t -> t -> NominalDiffTime
tickDiffTime c t1 t0 = timeOf c t1 - timeOf c t0

newtype ClockRate = MkClockRate Nat

type At8kHz = 'MkClockRate 8000

type At12kHz = 'MkClockRate 12000

type At16kHz = 'MkClockRate 16000

type At22050Hz = 'MkClockRate 22050

type At32kHz = 'MkClockRate 32000

type At44100Hz = 'MkClockRate 44100

type At48kHz = 'MkClockRate 48000

instance (KnownNat rate) =>
         HasClock (Proxy ('MkClockRate rate)) where
    type GetClock (Proxy ('MkClockRate rate)) = Clock
    type SetClock (Proxy ('MkClockRate rate)) t = Proxy ('MkClockRate rate)
    clock = lens (MkClock . fromInteger . natVal . getRate) const
      where
        getRate :: Proxy ('MkClockRate rate) -> Proxy rate
        getRate _ = Proxy
