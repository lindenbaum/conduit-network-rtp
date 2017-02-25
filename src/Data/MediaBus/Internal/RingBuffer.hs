-- | A circular buffer with a focus on high performance(TODO), useful e.g. for
-- bounded queues, e.g. in multimedia applications.
module Data.MediaBus.Internal.RingBuffer
    ( -- * Ring Buffer Data Type
      RingBuffer()
      -- ** Constructor
    , newRingBuffer
    , fromList
      -- ** Push Operations
    , push
    , tryPush
    , pushAll
    , pushOut
      -- ** Pop Operations
    , pop
    , pop_
    , popAndSet
    , tryPop
    , tryPopAndSet
    , popAll
      -- ** Size and Capacity Accessors
    , size
    , capacity
    , isFull
      -- ** FIFO Element accessor
    , firstElement
    , lastElement
      -- ** Ring Index accessor
    , startPosition
    , endPosition
      -- ** Ring Element allocation/deallocation
    , freeFirst
    , allocLast
    ) where

import           Control.Lens
import           Data.Array
import           Data.Default
import           Data.Function   ( on )
import           Data.List       ( unfoldr )
import           Text.Printf
import           GHC.Generics    ( Generic )
import           Control.DeepSeq

-- | A __bounded__ /FIFO container/ with @O(1)@ time- and space complexity back insertion
-- ('push') and front extraction ('pop').
data RingBuffer e = MkRingBuffer { ringBuffer :: Array Int e
                                 , startIndex :: Int
                                 , size       :: Int
                                 }
    deriving Generic

instance NFData e =>
         NFData (RingBuffer e)

instance Foldable RingBuffer where
    foldr f z = foldr f z . popAll

instance Eq e =>
         Eq (RingBuffer e) where
    (==) = (==) `on` popAll

instance Show e =>
         Show (RingBuffer e) where
    show r@MkRingBuffer{ringBuffer,startIndex,size} =
        printf "RingBuffer\n capacity: %d\n size: %d\n elements:\n%s\n"
               (capacity r)
               size
               (showElems (assocs ringBuffer))
      where
        showElems = unlines .
            map (\(k, v) -> printf "    %1s %1s %5s:  %s"
                                   (if k == r ^. endPosition
                                    then ">" :: String
                                    else "")
                                   (if k == startIndex
                                    then "<" :: String
                                    else "")
                                   (show k)
                                   (show v))

-- | Create a new 'RingBuffer' with the given 'capacity'. All elements are
-- initialized to 'def', hence the 'Default' constraint on the type parameter.
--
-- The ring capacity must not exceed @2^28@.
--
-- REPL recipes:
--
-- @@@
-- let r0 = newRingBuffer 10 :: RingBuffer Int
-- let r1 = r0 & lastElement .~ 10 & allocLast & lastElement .~ 11 & allocLast & lastElement .~ 12
-- r1 ^. firstElement
-- r1 & freeFirst
-- (r1 & freeFirst) ^. firstElement
-- @@@
newRingBuffer :: Default e => Int -> RingBuffer e
newRingBuffer n = fromList (replicate n def)

-- | Create an __empty__ 'RingBuffer' from a list of initial values, the length
-- of the list determines the 'capacity'.
--
-- The ring capacity must not exceed @2^28@.
fromList :: [e] -> RingBuffer e
fromList initialElements =
    let n = length initialElements
    in
        if n < 1 || n >= 2 ^ (28 :: Int)
        then error (printf "Invalid ring size: %d" n)
        else MkRingBuffer { ringBuffer = listArray (0, n - 1) initialElements
                          , startIndex = 0
                          , size = 0
                          }

-- | Add an element to end of the ring. If the ring 'isFull' the oldest element
-- will be overwritten and the next 'pop' will return the second oldest element.
push :: e -> RingBuffer e -> RingBuffer e
push e = allocLast . set lastElement e

-- | Add an element to the ring if it's not full.
tryPush :: e -> RingBuffer e -> Maybe (RingBuffer e)
tryPush e r
    | size r == capacity r = Nothing
    | otherwise = Just (push e r)

-- | Add all necessary elements of a given list to a ring, such that the last
-- element in the list is the last element 'pop'ped.
pushAll :: [e] -> RingBuffer e -> RingBuffer e
pushAll xs r = foldr push r (take (capacity r) (reverse xs))

-- | Add an element to the ring similar to 'push', but return a pair of the
-- 'Maybe' the element that is replaced if the ring is full and the new ring.
pushOut :: e -> RingBuffer e -> (Maybe e, RingBuffer e)
pushOut e r = let (o, r') = pop r
              in
                  if isFull r then (Just o, push e r') else (Nothing, push e r)

-- | Return a pair of the oldest element, and a new ring. If the ring is empty,
-- return the next element in the buffer, and adjust the ring such that the next
-- 'push' will be returned by the subsequent pop.
pop :: RingBuffer e -> (e, RingBuffer e)
pop = popAndModify (\x -> (x, Nothing))

-- | Update a ring as if 'pop' was called.
pop_ :: RingBuffer e -> RingBuffer e
pop_ = runIdentity . popAndModify (const (Identity Nothing))

-- | Return a pair of the oldest element, and a new ring. If the ring is empty,
-- return the next element in the buffer, and adjust the ring such that the next
-- 'push' will be returned by the subsequent pop.
--
-- The slot in the the ring from which an element was popped will contain the
-- given value in the returned ring buffer.
popAndSet :: e -> RingBuffer e -> (e, RingBuffer e)
popAndSet replacement = popAndModify (\poppedElement -> ( poppedElement
                                                        , Just replacement
                                                        ))

-- | Pop an element if the ring is not empty.
tryPop :: RingBuffer e -> Maybe (e, RingBuffer e)
tryPop r = if size r > 0 then Just (pop r) else Nothing

-- | Pop an element if the ring is not empty.
tryPopAndSet :: e -> RingBuffer e -> Maybe (e, RingBuffer e)
tryPopAndSet replacement ring =
    if size ring > 0 then Just (popAndSet replacement ring) else Nothing

-- | Return a list of all elements in the ring.
popAll :: RingBuffer e -> [e]
popAll = unfoldr tryPop

-- | Pop an element and maybe set a new value for the slot in the
-- ring from where the element was popped.
--
-- This function accepts any 'Functor' as the result of the operation. It is
-- used by the other @pop@ like functions.
popAndModify :: (Functor f)
             => (e -> f (Maybe e))
             -> RingBuffer e
             -> f (RingBuffer e)
popAndModify f ring = updateRing <$> f poppedElement
  where
    poppedElement = ring ^. firstElement
    updateRing = freeFirst . maybe ring (\e -> set firstElement e ring)

-- | Return the potential number of elements that could fit into the given ring.
capacity :: RingBuffer e -> Int
capacity = rangeSize . bounds . ringBuffer

-- | Return 'True' if the ring is not empty and a 'push' would change the result
-- of a subsequent 'pop'.
isFull :: RingBuffer e -> Bool
isFull r = size r == capacity r

-- | A 'Lens' for the element at 'startPosition'.
firstElement :: Lens' (RingBuffer e) e
firstElement f ring@MkRingBuffer{ringBuffer,startIndex} =
    replace <$> f (ringBuffer ! startIndex)
  where
    replace e = ring { ringBuffer = ringBuffer // [ (startIndex, e) ] }

-- | A 'Lens' for the element at 'endPosition'.
lastElement :: Lens' (RingBuffer e) e
lastElement f ring@MkRingBuffer{ringBuffer} =
    replace <$> f (ringBuffer ! (ring ^. endPosition))
  where
    replace e = ring { ringBuffer = ringBuffer //
                         [ (ring ^. endPosition, e) ]
                     }

-- | A 'Getter' for the position that points to the beginning of the elements
-- written to the ring buffer.
startPosition :: Getting Int (RingBuffer e) Int
startPosition = to startIndex

-- | A 'Getter' for the position that points to the first empty slot of the ring
-- buffer.
endPosition :: Getting Int (RingBuffer e) Int
endPosition = to (\r -> (size r + startIndex r) `rem` capacity r)

-- | Increase the 'startPosition', if necessary increase the 'endPosition' as
-- well, such that 'push'ing never stores elements before the 'pop' position as.
freeFirst :: RingBuffer e -> RingBuffer e
freeFirst ring@MkRingBuffer{startIndex,size} =
    ring { startIndex = startIndex', size = size' }
  where
    startIndex' = (startIndex + 1) `rem` n
    size' = max 0 (size - 1)
    n = capacity ring

-- | Increase the 'endPosition', if necessary move the 'startPosition' along as
-- well, such that 'pop'ping never skips over older ring elements.
allocLast :: RingBuffer e -> RingBuffer e
allocLast ring@MkRingBuffer{startIndex,size} =
    ring { startIndex = startIndex', size = size' }
  where
    startIndex' = if isFull ring then (startIndex + 1) `rem` n else startIndex
    size' = min n (size + 1)
    n = capacity ring
