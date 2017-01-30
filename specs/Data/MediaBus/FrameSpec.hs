module Data.MediaBus.FrameSpec ( spec ) where

import           Conduit
import           Test.Hspec
import           Data.MediaBus

spec :: Spec
spec = describe "Frames with an example FormatProxy as format" $
    it "exposes the mutable buffer of stereo samples" $
        pending
