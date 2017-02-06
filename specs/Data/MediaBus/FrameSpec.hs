module Data.MediaBus.FrameSpec ( spec ) where

import           Test.Hspec

spec :: Spec
spec = describe "Frame conduits" $
    describe "Functor instance: fmap" $
      it "should have the same result as 'overFrameContentC (...) f'" 
        pending
        -- fmap f = overFrameContentC undefined (mapInput _frameValue (const Nothing) (const f))
