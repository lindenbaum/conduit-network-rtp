-- | A circular buffer with a focus on high performance(TODO), useful e.g. for
-- bounded queues, e.g. in multimedia applications.
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}

module Data.MediaBus.RingBuffer
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
    ) where

import           Data.Array
import           Data.Default
import           Text.Printf
import           Data.Typeable
import           Data.List             ( unfoldr )
import           Data.Function         ( on )
import           Data.Functor.Identity

-- | A __bounded__ /FIFO container/ with @O(1)@ time- and space complexity back insertion
-- ('push') and front extraction ('pop').
data RingBuffer e = MkRingBuffer { ringBuffer :: Array Int e
                                 , readIndex  :: Int
                                 , writeIndex :: Int
                                 }
    deriving (Typeable)

instance Eq e =>
         Eq (RingBuffer e) where
    (==) = (==) `on` popAll

instance (Typeable e, Show e) =>
         Show (RingBuffer e) where
    show r@MkRingBuffer{ringBuffer,readIndex,writeIndex} =
        printf "Ring buffer for up to %d elements of type: %s\n  size: %d\nreadIndex: %d\n  writeIndex: %d(%d)\n  elements: %s\n"
               (capacity r)
               (show (typeRep r))
               (size r)
               readIndex
               (writeIndex `rem` capacity r)
               writeIndex
               (show (assocs ringBuffer))

-- | Create a new 'RingBuffer' with the given 'capacity'. All elements are
-- initialized to 'def', hence the 'Default' constraint on the type parameter.
--
-- The ring capacity must not exceed @2^28@.
newRingBuffer :: (Default e) => Int -> RingBuffer e
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
                          , readIndex = 0
                          , writeIndex = 0
                          }

-- | Add an element to end of the ring. If the ring 'isFull' the oldest element
-- will be overwritten and the next 'pop' will return the second oldest element.
push :: e -> RingBuffer e -> RingBuffer e
push e r@MkRingBuffer{ringBuffer,readIndex,writeIndex} =
    MkRingBuffer ringBuffer' readIndex' writeIndex'
  where
    n = capacity r
    ringBuffer' = ringBuffer // [ (writeIndex `rem` n, e) ]
    readIndex' = if isFull r then (readIndex + 1) `rem` n else readIndex
    writeIndex' = writeIndex + 1 -
        if writeIndex + 1 >= 2 * n then n else 0

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
popAndModify modify ring@MkRingBuffer{ringBuffer,readIndex,writeIndex} =
    updateRing <$> modify poppedElement
  where
    poppedElement = ringBuffer ! readIndex
    updateRing maybeReplacement =
        MkRingBuffer ringBuffer' readIndex' writeIndex'
      where
        ringBuffer' = maybe ringBuffer
                            (\e' -> ringBuffer // [ (readIndex, e') ])
                            maybeReplacement
        readIndex' = (readIndex + 1) `rem` n
        writeIndex' = let writeIndexMod = writeIndex `rem` n
                      in
                          if writeIndexMod == readIndex'
                          then readIndex'
                          else writeIndex
    n = capacity ring

-- | Return the number of elements in the ring, that were pushed but not popped,
-- but never more than the 'capacity' of the ring.
size :: RingBuffer e -> Int
size r@MkRingBuffer{readIndex,writeIndex} =
    let n = capacity r
    in
        if writeIndex == readIndex + n
        then n
        else let writeIndexMod = writeIndex `rem` n
             in
                 if readIndex <= writeIndexMod
                 then writeIndexMod - readIndex
                 else n - readIndex + writeIndexMod

-- | Return the potential number of elements that could fit into the given ring.
capacity :: RingBuffer e -> Int
capacity = rangeSize . bounds . ringBuffer

-- | Return 'True' if the ring is not empty and a 'push' would change the result
-- of a subsequent 'pop'.
isFull :: RingBuffer e -> Bool
isFull r = writeIndex r == readIndex r + capacity r
