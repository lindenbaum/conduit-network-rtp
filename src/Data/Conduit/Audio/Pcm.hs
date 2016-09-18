module Data.Conduit.Audio.Pcm
       (Pcm(..), pcmEmpty, pcmFromList, pcmLength, foldPcm, update)
       where

import           Data.Int
import qualified Data.Vector.Storable as V
import           Text.Printf

newtype Pcm = Pcm (V.Vector Int16)

-- | Create an empty 'Pcm'
pcmEmpty :: Pcm
pcmEmpty = Pcm V.empty

-- | Create a 'Pcm' from a list of samples.
pcmFromList :: [Int16] -> Pcm
pcmFromList = Pcm . V.fromList

-- | Return the length of the Pcm data.
pcmLength :: Pcm -> Int
pcmLength (Pcm !vec) = V.length vec

-- | Fold with index over the 'Pcm' data.
foldPcm
  :: (b -> Int -> Int16 -> b) -> b -> Pcm -> b
foldPcm !f !z (Pcm !vec) = V.ifoldl' f z vec

update
  :: Pcm -> [(Int,Int16)] -> Pcm
update (Pcm !vec) !us = Pcm (vec V.// us)

instance Show Pcm where
  show (Pcm !vec) =
    printf "Pcm buffer with %d samples:\n<< " (V.length vec)
    ++
    (if V.length vec > 10
     then unwords (printf "%0.4x" <$> V.toList (V.take 10 vec)) ++ " ..."
     else unwords (printf "%0.4x" <$> V.toList vec))
    ++  " >>\n"
