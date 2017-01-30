module Data.MediaBus.Internal.BiIndexSpec ( spec ) where

import           Data.MediaBus.Internal.BiIndex
import           Test.Hspec
import           Test.QuickCheck
import           Control.Lens

spec :: Spec
spec = describe "BiIndex consistency" $ do
    it "is consistent regarding the major component" $
        property additionMajor
    it "is consistent regarding the minor component" $
        property additionMinor

additionMajor :: BiIndex Integer Integer -> Integer -> Bool
additionMajor b x = let h = major b - majorOffset b + x
                    in
                        major (b & baseIndex +~ x) == h `modRange` majorRange b

additionMinor :: BiIndex Integer Integer -> Integer -> Bool
additionMinor b x = let h = minor b - minorOffset b + x
                    in
                        minor (b & baseIndex +~ x) == h `modRange` minorRange b
