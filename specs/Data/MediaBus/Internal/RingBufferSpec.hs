{-# LANGUAGE NoOverloadedStrings #-}

module Data.MediaBus.Internal.RingBufferSpec ( spec ) where

import           Data.MediaBus.Internal.RingBuffer
import           Test.Hspec
import           Test.QuickCheck
import           Data.Maybe
import           Data.Monoid
import           Data.Foldable            ( fold, foldMap )

spec :: Spec
spec = describe "RingBuffer" $ do
    specificExamples
    generalProperties

specificExamples = describe "specific examples" $ do
    let emptyRing :: RingBuffer Int
        emptyRing = newRingBuffer ringSize
        ringSize = 3
        singletonRing = push e0 emptyRing
        e0 = 123
    describe "tryPop" $ do
        it "returns Nothing for an initial, new ring buffer" $
            isNothing (tryPop emptyRing)
        it "returns a pair of the element and an empty ring buffer for a singleton ring buffer" $ do
            let Just (e, r) = tryPop singletonRing
            e `shouldBe` e0
            size r `shouldBe` 0
    describe "popAndSet" $ do
        it "replaces the popped value in the ring" $
            let ring = fromList [ "something" ]
            in
                popAndSet "replacement" ring `shouldBe`
                    ("something", fromList [ "replacement" ])
    describe "tryPopAndSet" $ do
        it "replaces the popped value in a non-empty ring" $
            let ring = push "something" $ fromList [ "" ]
            in
                tryPopAndSet "replacement" ring `shouldBe`
                    Just ("something", fromList [ "replacement" ])
    describe "pushAll" $ do
        it "pushes [1,2] into an empty ring such that pop returns 1" $
            fst (pop (pushAll [ 1, 2 ] emptyRing)) `shouldBe` 1
    describe "size" $ do
        it "returns 0 for an initial, new ring buffer" $
            size emptyRing `shouldBe` 0
        it "returns 1 for a singleton ring buffer" $
            size singletonRing `shouldBe` 1
        it "returns 0 for a ring buffer from which more elements were popped than the buffer contained" $
            size (pop_ $ pop_ $ pop_ $ pop_ $ singletonRing) `shouldBe` 0
        it "returns 1 after first filling a ring with size 3 with 4 elements, and then popping 2 elements" $
            size (pop_ $ pop_ $ push 4 $ push 3 $ push 2 $ push 1 emptyRing) `shouldBe`
                1
        it "returns 1 after first filling a ring with size 3 with 4 elements, popping 2 elements, pushing one element and popping one element" $
            size (pop_ $
                      push 5 $
                          pop_ $
                              pop_ $ push 4 $ push 3 $ push 2 $ push 1 emptyRing) `shouldBe`
                1
    describe "pop on a ring buffer with one element" $ do
        it "returns the element and an empty ring buffer" $ do
            let (e, r) = pop singletonRing
            e `shouldBe` e0
            size r `shouldBe` 0

generalProperties = describe "general properties" $ do
    describe "Foldable instance" $ do
        it "foldr f z t == appEndo (foldMap (Endo . f) t) z" $
            property $
                \(Positive ringSize) ringOps ->
                    let t :: RingBuffer Int
                        t = runRingOps ringOps $ newRingBuffer ringSize
                        f = (+)
                        z = 0
                    in
                        foldr f z t == appEndo (foldMap (Endo . f) t) z
        it "foldl f z t == appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z" $
            property $
                \(Positive ringSize) ringOps ->
                    let t :: RingBuffer Int
                        t = runRingOps ringOps $ newRingBuffer ringSize
                        f = (+) :: Int -> Int -> Int
                        z = 0 :: Int
                    in
                        foldl f z t ==
                            appEndo (getDual (foldMap (Dual . Endo . flip f) t))
                                    z
        it "fold = foldMap id" $
            property $
                \(Positive ringSize) ringOps ->
                    let t :: RingBuffer (First Int)
                        t = runRingOps ringOps $ newRingBuffer ringSize
                        f = (+) :: Int -> Int -> Int
                        z = 0 :: Int
                    in
                        fold t == foldMap id t

    describe "capacity" $
        it "always returns the parameter given to 'newRingBuffer'" $
            property $
                \(Positive ringSize) ringOps ->
                    let ring :: RingBuffer Int
                        ring = runRingOps ringOps $ newRingBuffer ringSize
                    in
                        capacity ring `shouldBe` ringSize
    describe "size" $ do
        it "returns the number elements pushed but not popped (modulo the total ring size)" $
            property $
                \(NonEmpty elementsToPush) (Positive ringSize) ->
                    let ring :: RingBuffer Int
                        ring = foldr push
                                     (newRingBuffer ringSize)
                                     elementsToPush
                    in
                        size ring `shouldBe`
                            (length elementsToPush `min` ringSize)
        it "is always in the range: 0 .. capacity" $
            property $
                \ringOps (Positive ringSize) -> do
                    let ring :: RingBuffer Int
                        ring = runRingOps ringOps $ newRingBuffer ringSize
                    size ring `shouldSatisfy` (>= 0)
                    size ring `shouldSatisfy` (<= capacity ring)
    describe "tryPush" $ do
        it "returns Nothing when the ring is full" $
            property $
                \ringOps e -> do
                    let ring :: RingBuffer Int
                        ring = runRingOps ringOps $ newRingBuffer 4
                    isFull ring ==> tryPush e ring `shouldSatisfy` isNothing
        it "returns 'Just' the same as 'push' when the ring is not full" $
            property $
                \ringOps e -> do
                    let ring :: RingBuffer Int
                        ring = pop_ $ runRingOps ringOps $ newRingBuffer 30
                    tryPush e ring `shouldBe` Just (push e ring)
    describe "pushOut" $ do
        it "returns '(Nothing, ...)' when the ring isn't full" $
            property $
                \ringOps e -> do
                    let ring :: RingBuffer Int
                        ring = runRingOps ringOps $ newRingBuffer 30
                    not (isFull ring) ==> pushOut e ring `shouldBe`
                        (Nothing, push e ring)
        it "returns '(Just lasteElement, ...)' when the ring is full" $
            property $
                \ringOps e -> do
                    let ring :: RingBuffer Int
                        ring = runRingOps ringOps $ newRingBuffer 3
                    isFull ring ==> pushOut e ring `shouldBe`
                        (Just (fst $ pop ring), push e ring)
    describe "pop" $ do
        it "never returns a full ring buffer" $
            property $
                \ringOps1 ringOps2 ringOps3 (Positive ringSize) ->
                    let ring = runRingOps (ringOps1 ++ ringOps2 ++ ringOps3) $
                            newRingBuffer ringSize
                        ring :: RingBuffer Int
                    in
                        pop_ ring `shouldSatisfy` (not . isFull)
        it "returns the oldest of the most recent elements after any number of pushes" $
            property $
                \(NonEmpty elementsToPush) (Positive ringSize) ->
                    let empty :: RingBuffer Int
                        empty = newRingBuffer ringSize
                        filled = foldr push empty elementsToPush
                        (popped, _) = pop filled
                    in
                        popped `shouldBe` last (take ringSize elementsToPush)
    describe "popAll" $
        it "returns the most recent elements after any number of pushes" $
            property $
                \(NonEmpty elementsToPush) ->
                    let empty :: RingBuffer Int
                        empty = newRingBuffer ringSize
                        ringSize = 3
                        filled = foldr push empty elementsToPush
                        popped = popAll filled
                    in
                        popped `shouldBe` reverse (take ringSize elementsToPush)
    describe "pushAll" $
        it "pushAll xs ring == foldr push ring (reverse xs)" $
            property $
                \(NonEmpty xs) -> let ring :: RingBuffer Int
                                      ring = newRingBuffer 3
                                  in
                                      pushAll xs ring `shouldBe`
                                          foldr push ring (reverse xs)

-- | Run a list of ring operations, where 'Just' corresponds to push and 'Nothing' to 'pop_'.
runRingOps :: [Either e e] -> RingBuffer e -> RingBuffer e
runRingOps ops ringBuffer =
    foldr applyOp ringBuffer ops
  where
    applyOp (Right e) intermediateRingBuffer =
        push e intermediateRingBuffer
    applyOp (Left _) intermediateRingBuffer =
        pop_ intermediateRingBuffer
