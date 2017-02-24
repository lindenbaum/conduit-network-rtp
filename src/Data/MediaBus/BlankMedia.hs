module Data.MediaBus.BlankMedia ( CanGenerateBlankMedia(..), CanBeBlank(..) ) where

import           Data.MediaBus.Clock
import           Data.Time.Clock
import           GHC.TypeLits
import           Control.Lens

class CanGenerateBlankMedia a where
    blankFor :: NominalDiffTime -> a
    blankFor dt = blankForTicks (nominalDiffTime # dt :: Ticks 1000000000000 Integer)
    blankForTicks :: (Integral i, KnownNat r) => Ticks r i -> a
    blankForTicks ticks = blankFor (from nominalDiffTime # ticks)

class CanBeBlank a where
    blank :: a
