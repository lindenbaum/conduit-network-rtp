module Data.MediaBusSpec ( spec ) where

import           Conduit           as C
import           Data.Conduit.List ( consume, sourceList )
import           Data.List         ( sort )
import           Data.Word
import           Test.Hspec
import           Test.QuickCheck
import           Data.MediaBus

-- ----------------------------------------------------------------------
-- * Rtp Prototype
-- ----------------------------------------------------------------------
newtype RtpSsrc = MkRtpSsrc Word32
    deriving (Show, Eq)

newtype RtpSeqNum = MkRtpSeqNum Word16
    deriving (Show, Bounded, Integral, Num, Enum, Real, Ord, Eq)

newtype RtpTicks = MkRtpTicks Word32
    deriving (Show, Bounded, Integral, Num, Enum, Real, Ord, Eq)

data RtpPayload = MkRtpPayload { rtpPayload     :: String
                               , rtpPayloadType :: RtpPayloadType
                               }
    deriving Show

newtype RtpPayloadType = MkRtpPayloadType { fromRtpPayloadType :: Word8 }
    deriving (Show, Eq, Num)

data RawRtpPacket = MkRawRtpPacket { rawRtpSeqNumNumber :: RtpSeqNum
                                   , rawRtpTicks    :: RtpTicks
                                   , rawRtpSsrc         :: RtpSsrc
                                   , rawRtpPayload      :: RtpPayload
                                   }
    deriving Show

-- -----------------------------------------------------------------------------
-- * Tests/Specs
-- -----------------------------------------------------------------------------
spec :: Spec
spec = do
    reorderSpec
    synchronizeToSeqNumSpec

instance HasTicks Word8 where
    type GetTicks Word8 = Word8
    type SetTicks Word8 t = t
    ticks = ($)

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
                    inFrames = [ 253, 254, 255, 0, 1 :: Word8 ]
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
        SynchronizeTo (MkReference startVal)
                      (MkEvent (MkSeqNum startVal) (head xs))
    (rest `zip` drop 1 rest) `shouldSatisfy`
        all (not .
                 uncurry succeeds)
