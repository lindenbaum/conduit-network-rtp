module Data.MediaBus.RtpSpec ( spec ) where

import           Conduit
import           Data.Conduit.List
import           Data.MediaBus
import           Data.MediaBus.Internal.Series
import           Data.MediaBus.Internal.Conduit
import           Test.Hspec
import qualified Data.MediaBus.Rtp.Packet       as Rtp
import qualified Data.ByteString                as B

import Data.Proxy
import GHC.TypeLits

spec :: Spec
spec = describe "rtpSource" $ do
    let countStarts :: [Series a b] -> Int
        countStarts = foldr (\x n -> case x of
                                 Start _ -> n + 1
                                 Next _ -> n)
                            0
        runTestConduit inputs = runConduitPure (sourceList inputs .|
                                                    rtpSource .|
                                                    consume)

    it "yields 'Start' when only when the first payload packet arrives" $
        let inputs = [ MkStream (Start (MkFrameCtx 0 0 0)) ]
        in
            countStarts (_stream <$> runTestConduit inputs) `shouldBe`
                0

    it "yields 'Start' when the first packet arrives" $
        let inputs = [ MkStream (Start (MkFrameCtx 0 0 0))
                     , mkTestRtpPacket 0 0 0
                     ]
        in
            countStarts (_stream <$> runTestConduit inputs) `shouldBe`
                1

    it "yields 'Start' when the ssrc changes" $
        let inputs = [ MkStream (Start (MkFrameCtx 8 9 10))
                     , mkTestRtpPacket ssrc0 0 0
                     , mkTestRtpPacket ssrc0 0 0
                     , mkTestRtpPacket ssrc1 0 0
                     , mkTestRtpPacket ssrc1 0 0
                     , mkTestRtpPacket ssrc0 0 0
                     , mkTestRtpPacket ssrc1 0 0
                     , mkTestRtpPacket ssrc1 0 0
                     ]
            ssrc0 = 123
            ssrc1 = 345
        in
            countStarts (_stream <$> runTestConduit inputs) `shouldBe`
                4

    it "yields 'Start' when the sequence numbers change too much" $
        let inputs = [ MkStream (Start (MkFrameCtx 0 0 0))
                     , mkTestRtpPacket 0 01 0
                     , mkTestRtpPacket 0 02 0
                     , mkTestRtpPacket 0 03 0
                     , mkTestRtpPacket 0 24 0
                     , mkTestRtpPacket 0 25 0
                     , mkTestRtpPacket 0 06 0
                     , mkTestRtpPacket 0 07 0
                     ]
        in
            countStarts (_stream <$> runTestConduit inputs) `shouldBe`
                3

    it "yields 'Start' when the sequence numbers change too much" $
        let inputs = [ MkStream (Start (MkFrameCtx 0 0 0))
                     , mkTestRtpPacket 0 01 0
                     , mkTestRtpPacket 0 02 0
                     , mkTestRtpPacket 0 03 0
                     , mkTestRtpPacket 0 24 0
                     , mkTestRtpPacket 0 25 0
                     , mkTestRtpPacket 0 06 0
                     , mkTestRtpPacket 0 07 0
                     ]
        in
            countStarts (_stream <$> runTestConduit inputs) `shouldBe`
                3

    it "yields no 'Start' when the sequence number wraps around" $
        let inputs = [ MkStream (Start (MkFrameCtx 0 0 0))
                     , mkTestRtpPacket 0 (negate 3) 0
                     , mkTestRtpPacket 0 (negate 2) 0
                     , mkTestRtpPacket 0 (negate 1) 0
                     , mkTestRtpPacket 0 0 0
                     , mkTestRtpPacket 0 1 0
                     ]
        in
            countStarts (_stream <$> runTestConduit inputs) `shouldBe`
                1

    it "yields no 'Start' when the timestamp wraps around" $
        let inputs = [ MkStream (Start (MkFrameCtx 0 0 0))
                     , mkTestRtpPacket 0 0 (negate 300)
                     , mkTestRtpPacket 0 0 (negate 200)
                     , mkTestRtpPacket 0 0 (negate 100)
                     , mkTestRtpPacket 0 0 0
                     , mkTestRtpPacket 0 0 100
                     ]
        in
            countStarts (_stream <$> runTestConduit inputs) `shouldBe`
                1

    it "can handle broken packets without crashing" $
        let inputs = [ MkStream (Start (MkFrameCtx 0 0 0))
                     , mkTestRtpPacket 0 0 777
                     , mkTestRtpPacket 0 0 777
                     , mkBrokenTestRtpPacket
                     , mkBrokenTestRtpPacket
                     , mkTestRtpPacket 0 0 777
                     , mkTestRtpPacket 0 0 777
                     , mkBrokenTestRtpPacket
                     , mkTestRtpPacket 0 0 777
                     , mkTestRtpPacket 0 0 777
                     , mkTestRtpPacket 0 0 777
                     ]
        in
            length (runTestConduit inputs) `shouldBe`
                8

mkBrokenTestRtpPacket :: Stream Int Int Int B.ByteString
mkBrokenTestRtpPacket = MkStream (Next (MkFrame 0 0 (B.pack [0,0,0])))

mkTestRtpPacket :: Rtp.RtpSsrc
                -> Rtp.RtpSeqNum
                -> Rtp.RtpTimestamp
                -> Stream Int Int Int B.ByteString
mkTestRtpPacket ssrc sn ts =
    MkStream (Next (MkFrame 0
                            0
                            (Rtp.serialize (Rtp.MkRtpPacket (Rtp.MkRtpHeader 2
                                                                             False
                                                                             False
                                                                             sn
                                                                             ts
                                                                             ssrc
                                                                             []
                                                                             Nothing)
                                                            (Rtp.MkRtpPayload 0
                                                                              (sampleBufferFromList [ 0
                                                                                                    , 0
                                                                                                    , 0
                                                                                                    ]))))))

_receiveRtpFromUDP ::  IO [RtpStream]
_receiveRtpFromUDP = runConduitRes (udpDatagramSource useUtcClock 10000 "127.0.0.1" .|
    rtpSource .|
    dbgShowSink 0.001 "RTP")

-- _receiveRtpFromUDPAndDecodeAndPlayback ::  IO [RtpStream]
-- _receiveRtpFromUDPAndDecodeAndPlayback = runConduitRes (udpDatagramSource useUtcClock 10000 "127.0.0.1" .|
--     rtpSource .| dbgShowC 0.01 "RTP"
--     .| rtpPayloadDemux [(8, Proxy :: Proxy (Stream Rtp.RtpSsrc Rtp.RtpSeqNum (Ticks (Timing 8000 Word32)) (SampleBuffer ALaw)))])


-- TODO use the SourceId as a enriched stream config type, which contains the ssrc but also ptime, etc...

-- rtpPayloadDemux
--    :: Monad m => [(Word8, StreamConfig)] -> (forall t c. IsTiming t =>  Conduit (Stream SourceId' SeqNum' t (SampleBuffer c)) m out) -> Conduit RtpStream m out
-- rtpPayloadDemux  payloadTable sink = undefined


-- rtpPayloadMux
--   :: (IsTiming t, Monad m) => [(Word8, StreamConfig)] ->

-- type MyCfg = (    PayloadType 0  :=> Stream RtpSsrc RtpSeqNum (Ticks' 8000) (SampleBuffer ALaw)
--              :<>: PayloadType 97 :=> Stream RtpSsrc RtpSeqNum (Ticks' 8000) (SampleBuffer ALaw))
