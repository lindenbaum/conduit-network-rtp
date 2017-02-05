module Data.MediaBus.SequenceSpec ( spec ) where

import           Conduit           as C
import           Data.Conduit.List ( consume, sourceList )
import           Data.List         ( sort )
import           Data.Word
import           Test.Hspec
import           Test.QuickCheck
import           Data.MediaBus

-- -----------------------------------------------------------------------------
-- * Tests/Specs
-- -----------------------------------------------------------------------------
spec :: Spec
spec = do
    reorderSpec
    synchronizeToSeqNumSpec

reorderSpec :: Spec
reorderSpec = describe "reorder" $ do
    it "the output is always monotonic increasing" $
        property reorderOutputIsMonotoneIncreasing
    it "if the input is non-empty the output is too" $
        property reorderOutputOnlyEmptyIfInputEmpty
    it "works even if the index wraps around" $
        property $
            \(Positive windowSize) ->
                let outFrames = runConduitPure (sourceList inFrames .|
                                                    reorder windowSize .|
                                                    consume)
                    inFrames = [ 253, 254, 255, 0, 1 :: SeqNum Word8 ]
                in
                    outFrames `shouldBe` inFrames

reorderOutputIsMonotoneIncreasing :: [SeqNum Word8]
                                  -> Positive Int
                                  -> Expectation
reorderOutputIsMonotoneIncreasing inFrames (Positive windowSize) =
    let outFrames = runConduitPure (sourceList inFrames .|
                                        reorder windowSize .|
                                        consume)
    in
        outFrames `shouldBe` sort outFrames

reorderOutputOnlyEmptyIfInputEmpty :: [SeqNum Word8]
                                   -> Positive Int
                                   -> Expectation
reorderOutputOnlyEmptyIfInputEmpty inFrames (Positive windowSize) =
    let outFrames = runConduitPure (sourceList inFrames .|
                                        reorder windowSize .|
                                        consume)
    in
        null inFrames `shouldBe` null outFrames

synchronizeToSeqNumSpec :: Spec
synchronizeToSeqNumSpec =
    describe "synchronizeToSeqNum" $
        it "produces dense, strictly monotonic output" $
            property synchronizeToSeqNumIsMonotone

synchronizeToSeqNumIsMonotone :: (NonEmptyList [Bool]) -> Word64 -> Expectation
synchronizeToSeqNumIsMonotone (NonEmpty xs) startVal = do
    let inEvents = sourceList xs
        (first : rest) = runConduitPure (inEvents .|
                                             synchronizeToSeqNum (MkReference startVal) .|
                                             consume)
    first `shouldBe`
        ReSync (MkReference startVal) (MkSeqNum startVal) (head xs)
    (rest `zip` drop 1 rest) `shouldSatisfy`
        all (not .
                 uncurry succeeds)
