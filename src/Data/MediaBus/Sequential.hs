-- | Reorder out of order Rtp packets and detect gaps in the sequence numbers.
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveFoldable #-}

module Data.MediaBus.Sequential where

-- import qualified Data.Map as Map
import Control.Arrow
import qualified Data.Set as Set
-- import qualified Data.Ix as Ix
-- import Data.Word
-- import           Text.Printf

{- | Element of a sequence.

A sequence is an ordered set of 'Element's. An 'Element' has a /position/ or
index and a /content/ it refers to.

The @index@ field marks an elements position in the sequence.

Entries maybe missing in a sequence and this is indicated by using 'Nothing' for
the value in the @content@ field.

This might be required in order to enable features like generating RTCP or PLC.
-}
data Element s c = Element { index   :: s
                           , content :: Maybe c
                           }
    deriving (Foldable, Functor, Show)

instance Eq s => Eq (Element s c) where
  Element{index=i1} == Element{index=i2} = i1 == i2


instance Ord s => Ord (Element s c) where
  Element{index=i1} <= Element{index=i2} = i1 <= i2

{- | Insert a sequence 'Element' into the queue. It can be retrieved using 'next'.

The queue is bounded and stateful. Enqueueing an element has these semantics:

If the 'Element' has 'Position' 'Beginning' and the 'Queue' is currently empty,
the element will be enqueued and the queue will expect the next 'Element' to have
a sequence number higher than the given sequence as long as it is /related/.
-}
enqueue :: Ord s => Element s c -> Queue s c -> Queue s c
enqueue e c@Queue{queuedElements} =
    c { queuedElements = Set.insert e queuedElements }

next :: Queue s c -> (Maybe (Element s c), Queue s c)
next q@Queue{queuedElements} =
    let (res, queuedElements') = maybe (Nothing, queuedElements)
                                      (first Just)
                                      (Set.minView queuedElements)
    in
        (res, q { queuedElements = queuedElements' })

new :: Queue s c
new = Queue Set.empty

-- | The buffer and the state required for reordering and gap detection.
data Queue s b = Queue { queuedElements :: Set.Set (Element s b) }

instance Show s => Show (Queue s b) where
  show Queue{queuedElements} = "⧼" ++ unwords (show . index <$> Set.toList queuedElements) ++  "⧽"
