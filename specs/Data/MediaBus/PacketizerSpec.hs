module Data.MediaBus.PacketizerSpec ( spec ) where

import           Test.Hspec
import           Data.Conduit.List
import           Conduit
import           Test.QuickCheck
import           Data.MediaBus
import           Data.MediaBus.Internal.Series
import           Data.Word
import qualified Data.Vector.Storable          as V
import           Control.Lens

spec :: Spec
spec = describe "repacketizeC" $ do
    it "leaves intact a stream of frames which already has the desired packet duration" $
        property inputPacketsAlreadyHaveTheDesiredDuration
    it "returns k times as many packets of the input packet duration is k * the output packet duration" $
        property inputPacketsHaveAnIntegralMultipleOfTheDesiredDuration
    it "splits k packets of length 25 into 3 * l packets of length 10" $
        property inputPacketsAreBiggerAndNotDivisibleByTheDesiredDuration
    it "the sequence numbers of all output frames are strictly monotonic increasing, if the input packets already have the desired packet duration" $
        property inputPacketsAlreadyHaveTheDesiredDurationSeqnum
    it "the sequence numbers of all output frames are strictly monotonic increasing, if the input packets are split into more packets" $
        property inputPacketsHaveAnIntegralMultipleOfTheDesiredDurationSeqnum
    it "adapts the time stamps, when the input packets are split into more packets, such that the new duration is reflected in the frame time stamps" $
        property inputPacketsHaveAnIntegralMultipleOfTheDesiredDurationTicks

inputPacketsAlreadyHaveTheDesiredDuration (Positive len) count =
    let inputs = mkTestStartPacket 0 len : [ mkTestPacket n len
                 | n <- [0 .. count] ]
    in
        length (runRepacketize inputs len) `shouldBe` length inputs

inputPacketsHaveAnIntegralMultipleOfTheDesiredDuration (Positive len) count (Positive mult) =
    let inputs = [ mkTestPacket n (len * mult)
                 | n <- [0 .. count] ]
    in
        (length (runRepacketize inputs len) `div` mult) `shouldBe` length inputs

inputPacketsAreBiggerAndNotDivisibleByTheDesiredDuration (Positive count) =
    let inputs = mkTestStartPacket 0 25 : [ mkTestPacket n 25
                 | n <- [0 .. count] ]
    in
        length (runRepacketize inputs (10 :: Word8)) `shouldBe` length inputs +
            2 * (fromIntegral count + 1)

inputPacketsAlreadyHaveTheDesiredDurationSeqnum (Positive len) count =
    let inputs = mkTestStartPacket 0 len : [ mkTestPacket n len
                 | n <- [0 .. count] ]
    in
        seqNumStrictlyMonotoneIncreasing $ runRepacketize inputs len

inputPacketsHaveAnIntegralMultipleOfTheDesiredDurationSeqnum (Positive len) count (Positive mult) =
    let inputs = mkTestStartPacket 0 (len * mult) : [ mkTestPacket n (len * mult)
                 | n <- [0 .. count] ]
    in
        seqNumStrictlyMonotoneIncreasing $ runRepacketize inputs len

inputPacketsHaveAnIntegralMultipleOfTheDesiredDurationTicks (Positive len) count (Positive mult) =
    let inputs = mkTestStartPacket 0 (len * mult) : [ mkTestPacket n (len * mult)
                 | n <- [0 .. count] ]
    in
        ticksStrictlyMonotoneIncreasing (fromIntegral len) $
            runRepacketize inputs len

seqNumStrictlyMonotoneIncreasing outs =
    let res = view seqNum <$> outs
    in
        zipWith (-) (Prelude.drop 2 res) (Prelude.drop 1 res) `shouldSatisfy` all (== 1)

ticksStrictlyMonotoneIncreasing dur outs =
    let res = view timestamp' <$> outs
    in
        zipWith (-) (Prelude.drop 2 res) (Prelude.drop 1 res) `shouldSatisfy` all (== dur)

runRepacketize inputs len =
    runConduitPure (sourceList inputs .|
                        repacketizeC (fromIntegral len / 8000) .|
                        consume)

mkTestPacket :: Word8
             -> Int
             -> Stream () Word8 (Ticks 8000 Word32) (SampleBuffer (S16 8000))
mkTestPacket sn len = MkStream (Next (MkFrame (MkTicks (fromIntegral sn *
                                                            fromIntegral len))
                                              sn
                                              (MkSampleBuffer (V.replicate len 0))))

mkTestStartPacket :: Word8
                  -> Int
                  -> Stream () Word8 (Ticks 8000 Word32) (SampleBuffer (S16 8000))
mkTestStartPacket sn len =
    MkStream (Start (MkFrameCtx ()
                                (MkTicks (fromIntegral sn * fromIntegral len))
                                sn))
