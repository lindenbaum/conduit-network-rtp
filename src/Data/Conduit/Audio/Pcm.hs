module Data.MediaBus.Audio.Pcm
       ( Pcm(..), RawPcm
       , Pcm8KMono, Pcm16KMono, Pcm48KStereo
       , PcmFormat(..), PcmChannels(..), PcmRate(..)
       -- , pcmRateVal
       -- , pcmRate
       , pcmChannelsVal
       , pcmChannels
       , pcmEmpty, pcmFromList, pcmLength, foldPcm, update, pcmModify)
       where

import           Data.Int
import qualified Data.Vector.Storable as V
import           Text.Printf
import           GHC.TypeLits
import           Data.Proxy
import           Control.Monad.ST (ST)

-- | Pulse coded audio samples annotated with a type indicating the format
newtype Pcm (i :: PcmFormat) = Pcm RawPcm

-- | Pulse coded audio samples, each represented as 'Int16'
type RawPcm = V.Vector Int16

-- | High performance modification (possibly in-place)
pcmModify :: (forall s. V.MVector s Int16 -> ST s ()) -> Pcm ('MkPcmFormat c r) -> Pcm ('MkPcmFormat c' r')
pcmModify f (Pcm !vin) = Pcm (V.modify f vin)

-- | Pulse coded audio sample format
data PcmFormat = MkPcmFormat PcmChannels PcmRate

type Pcm8KMono = Pcm ('MkPcmFormat 'PcmMono 'Pcm8000)
type Pcm16KMono = Pcm ('MkPcmFormat 'PcmMono 'Pcm16000)
type Pcm48KStereo = Pcm ('MkPcmFormat 'PcmStereo 'Pcm48000)

-- | Pulse coded audio sample interleaving is determined by the number of
-- channels, so this information must be present in 'PcmFormat'
data PcmChannels = PcmMono | PcmStereo

pcmChannels :: forall (c :: PcmChannels) r.
            KnownNat (GetPcmChannels c)
            => Pcm ('MkPcmFormat c r)
            -> Int
pcmChannels _ = pcmChannelsVal (Proxy @c)

pcmChannelsVal
  :: forall (c :: PcmChannels) . KnownNat (GetPcmChannels c) =>  Proxy c -> Int
pcmChannelsVal _ = fromIntegral (natVal (Proxy @(GetPcmChannels c)))

type family GetPcmChannels (c :: PcmChannels) :: Nat where
  GetPcmChannels 'PcmMono = 1
  GetPcmChannels 'PcmStereo = 2

-- | Pulse coded audio sample rate.
data PcmRate = Pcm8000 | Pcm16000 | Pcm48000

pcmRate
  :: forall c (r :: PcmRate) . KnownNat (GetPcmRate r) =>  Pcm ('MkPcmFormat c r) -> Int
pcmRate _ = pcmRateVal (Proxy @r)

pcmRateVal
  :: forall (c :: PcmRate) . KnownNat (GetPcmRate c) =>  Proxy c -> Int
pcmRateVal _ = fromIntegral (natVal (Proxy @(GetPcmRate c)))

type family GetPcmRate (c :: PcmRate) :: Nat where
  GetPcmRate 'Pcm8000 = 8000
  GetPcmRate 'Pcm16000 = 16000
  GetPcmRate 'Pcm48000 = 48000

-- | Create an empty 'Pcm'
pcmEmpty :: Pcm i
pcmEmpty = Pcm V.empty

-- | Create a 'Pcm' from a list of samples.
pcmFromList :: [Int16] -> Pcm i
pcmFromList = Pcm . V.fromList

-- | Return the number of samples in the Pcm data, regardless of number of
-- channels, if this is a stereo buffer the length must be divded by two.
pcmLength :: Pcm i -> Int
pcmLength (Pcm !vec) = V.length vec

-- | Fold with index over the 'Pcm' data.
foldPcm
  :: (b -> Int -> Int16 -> b) -> b -> Pcm i -> b
foldPcm !f !z (Pcm !vec) = V.ifoldl' f z vec

update
  :: Pcm i -> [(Int,Int16)] -> Pcm i
update (Pcm !vec) !us = Pcm (vec V.// us)

instance (KnownNat (GetPcmChannels ch), KnownNat (GetPcmRate rate)) => Show (Pcm ('MkPcmFormat ch rate)) where
  show (Pcm !vec) =
    printf "PCM %d/%d/%d << "
    (pcmRateVal (Proxy @rate))
    (pcmChannelsVal (Proxy @ch))
    (V.length vec)
    ++
    (if V.length vec > 10
     then unwords (printf "%0.4x" <$> V.toList (V.take 10 vec)) ++ " ..."
     else unwords (printf "%0.4x" <$> V.toList vec))
    ++  " >>\n"
