module Data.MediaBus.BufferSpec ( spec ) where

import           Data.MediaBus
import           Control.Monad
import           Test.Hspec
import           Control.Lens
import           Data.Char
import qualified Data.Vector.Generic.Mutable as V

spec :: Spec
spec = describe "SampleBuffer" $ do
    it "can be mapped over with eachSample" $
        ((MkSampleBuffer (fromList "Hello")) & eachSample %~ toUpper) `shouldBe`
        MkSampleBuffer (fromList "HELLO")
    it "can be mapped over with eachSample changing the type" $
        (MkSampleBuffer (fromList "Hello") & sampleBuffer . sampleVector .
             each %~
             const True) `shouldBe`
        MkSampleBuffer (fromList (Prelude.replicate 5 True))
    describe "mutateSamples" $
        it "modifies in-place" $
        let f v =
                -- imperative safe destructive updates
                let n = V.length v
                in
                    forM_ [0 .. (n - 1) `div` 2] (\i -> V.swap v i (n - 1 - i))
        in
            mutateSamples f (MkSampleBuffer (fromList [1 .. 4 :: Int])) `shouldBe`
                MkSampleBuffer (fromList [4,3 .. 1])
    describe "unsafeMutateSamples" $
        it "modifies in-place and can return values" $
        let f v =
                -- imperative safe destructive updates
                let n = V.length v
                in
                    forM [0 .. (n - 1) `div` 2]
                         (\i -> do
                              V.swap v i (n - 1 - i)
                              return i)
        in
            unsafeMutateSamples f
                                (MkSampleBuffer (fromList [1 .. 4 :: Int])) `shouldBe`
                ([ 0, 1 ], MkSampleBuffer (fromList [4,3 .. 1]))

data TestFormat = MkTestFormat
    deriving Show
