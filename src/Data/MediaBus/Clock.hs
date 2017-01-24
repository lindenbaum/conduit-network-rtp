module Data.MediaBus.Clock
    ( IsClock(..)
    , toRate
    , clockDiffTime
    , StaticClock(..)
    , Clock(..)
    , fromRate
    ) where

import           Data.Time.Clock
import           GHC.TypeLits

class IsClock c where
    tickDuration :: c -> NominalDiffTime

    -- | Return the number of timestamp that a temopral value represents
    timeOf :: Integral t => c -> t -> NominalDiffTime
    timeOf c t = fromInteger (toInteger t) * tickDuration c

toRate :: (IsClock c, Integral t) => c -> t
toRate c = round (recip (timeOf c (1 :: Integer)))

clockDiffTime :: (IsClock c, Integral t) => c -> t -> t -> NominalDiffTime
clockDiffTime c t1 t0 = timeOf c t1 - timeOf c t0

-- --------------------------------------
data StaticClock (rate :: Nat) = MkStaticClock

instance (KnownNat rate) =>
         IsClock (StaticClock rate) where
    tickDuration = fromInteger . natVal

-- | A clock
newtype Clock = MkClock NominalDiffTime
    deriving (Show)

fromRate :: Integral t => t -> Clock
fromRate timestampPerSecond =
    MkClock (1 / fromIntegral timestampPerSecond)
