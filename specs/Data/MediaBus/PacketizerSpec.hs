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
    it "return k times as many packets of the input packet duration is k * the output packet duration" $
        property inputPacketsHaveAnIntegralMultipleOfTheDesiredDuration
    it "splits k packets of length 25 into 3 * l packets of length 10" $
        property inputPacketsAreBiggerAndNotDivisibleByTheDesiredDuration
    it "the sequence numbers of all output frames are strictly monotonic increasing, if the input packets already have the desired packet duration" $
        property inputPacketsAlreadyHaveTheDesiredDurationSeqnum
    it "the sequence numbers of all output frames are strictly monotonic increasing, if the input packets are split into more packets" $
        property inputPacketsHaveAnIntegralMultipleOfTheDesiredDurationSeqnum

inputPacketsAlreadyHaveTheDesiredDuration (Positive len) count =
    let inputs = [ mkTestPacket n len
                 | n <- [0 .. count] ]
    in
        length (outputs inputs len) `shouldBe` length inputs

inputPacketsHaveAnIntegralMultipleOfTheDesiredDuration (Positive len) count (Positive mult) =
    let inputs = [ mkTestPacket n (len * mult)
                 | n <- [0 .. count] ]
    in
        (length (outputs inputs len) `div` mult) `shouldBe` length inputs

inputPacketsAreBiggerAndNotDivisibleByTheDesiredDuration (Positive count) =
    let inputs = [ mkTestPacket n 25
                 | n <- [0 .. count] ]
    in
        length (outputs inputs (10 :: Word8)) `shouldBe` length inputs +
            2 * (fromIntegral count + 1)

inputPacketsAlreadyHaveTheDesiredDurationSeqnum (Positive len) count =
    let inputs = [ mkTestPacket n len
                 | n <- [0 .. count] ]
    in
        seqNumStrictlyMonotoneIncreasing $ outputs inputs len

inputPacketsHaveAnIntegralMultipleOfTheDesiredDurationSeqnum (Positive len) count (Positive mult) =
    let inputs = [ mkTestPacket n (len * mult)
                 | n <- [0 .. count] ]
    in
        seqNumStrictlyMonotoneIncreasing $ outputs inputs len

seqNumStrictlyMonotoneIncreasing outs =
    let res = view seqNum <$> outs
    in
        zipWith (-) (Prelude.drop 1 res) res `shouldSatisfy` all (== 1)

outputs inputs len = runConduitPure (sourceList inputs .|
                                         repacketizeC (fromIntegral len) .|
                                         consume)

mkTestPacket :: Word8 -> Int -> Stream () Word8 Word8 (SampleBuffer (S16 1))
mkTestPacket sn len = MkStream (Next (MkFrame sn
                                              sn
                                              (MkSampleBuffer (V.replicate len 0))))
