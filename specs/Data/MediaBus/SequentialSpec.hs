module Data.MediaBus.SequentialSpec(spec) where

import Data.MediaBus.Sequential
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Queue" $ do
  let q0 :: Queue Int ()
      q0 = new
  describe (show q0) $ do
    describe "next" $ do
        it "returns nothing"
           (fst (next q0) `shouldBe` Nothing)
  let q1 = enqueue e q0
      e = Element 0 Nothing
  describe (show q1) $ do
    describe "next" $ do
        it "returns the element"
           (fst (next q1) `shouldBe` Just e)
