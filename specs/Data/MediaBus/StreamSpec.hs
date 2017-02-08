module Data.MediaBus.StreamSpec ( spec ) where

import           Test.Hspec
import           Test.QuickCheck
import           Conduit
import           Data.Conduit.List
import           Data.MediaBus
import           Data.Word
import           Control.Monad
import           Data.MediaBus.Internal.Conduit

spec :: Spec
spec = describe "Stream conduits" $
    describe "Functor instance: fmap" $
        it "should have the same result as 'overFrameContentC (...) f'" pending

-- fmap f = overFrameContentC undefined (mapInput _frameValue (const Nothing) (const f))
_helloWorld :: IO ()
_helloWorld = void $
    runConduit (yieldMany ("Hello world" :: String) .|
                    (synchronizeToClock :: Conduit Char IO (ClockSynced UtcClock Char)) .|
                    dbgShowC 1 "YO" .|
                    consume)

_sampleSomeStream :: IO [Stream' 8000 Bool]
_sampleSomeStream = join <$> sample' (resize 30 (listOf1 arbitrary))

_yieldStream :: Monad m => [Stream' 8000 Bool] -> Source m (Stream' 8000 Bool)
_yieldStream frames = yieldMany frames .|
    dbgShowC 1 "ORIGINAL"

_reorderSomeFrames = void $
    _sampleSomeStream >>=
        \fs -> runConduit (_yieldStream fs .|
                               overStreamC (reorderSeries (\(MkFrame t s _) -> MkFrameCtx 0
                                                                                          t
                                                                                          s)
                                                          2 .|
                                                dbgShowC 1 "ORDERED") .|
                               consume)
