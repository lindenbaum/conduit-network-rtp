module Data.MediaBus.Discontinous
    ( Discontinous(..)
    , _Missing
    , _Got
    , concealMissing
    , type DiscontinousWithSTicks
    ) where

import           Conduit
import           Control.Lens
import           Control.Parallel.Strategies ( NFData )
import           Data.Default
import           Data.MediaBus.Clock
import           Data.MediaBus.Payload
import           Data.MediaBus.Stream
import           Data.Time.Clock
import           GHC.Generics                ( Generic )

--  TODO create a gap detection mechanism, a simple stateful conduit that knows the next timestamp
data Discontinous a = Missing !NominalDiffTime
                    | Got !a
    deriving (Show, Generic)

instance NFData a =>
         NFData (Discontinous a)

instance Default (Discontinous a) where
    def = Missing 1

makePrisms ''Discontinous

instance HasDuration a =>
         HasDuration (Discontinous a) where
    getDuration (Missing !d) =
        d
    getDuration (Got !x) = getDuration x

instance HasPayload a =>
         HasPayload (Discontinous a) where
    type GetPayload (Discontinous a) = GetPayload a
    type SetPayload (Discontinous a) b = Discontinous (SetPayload a b)
    payload = _Got . payload

concealMissing :: (NFData c, Monad m)
               => (NominalDiffTime -> c)
               -> Conduit (Stream i s t (Discontinous c)) m (Stream i s t c)
concealMissing concealF =
    mapPayloadC' go
  where
    go (Got !b) = b
    go (Missing !dur) = concealF dur

type DiscontinousWithSTicks r t x = WithSTicks r t (Discontinous x)
