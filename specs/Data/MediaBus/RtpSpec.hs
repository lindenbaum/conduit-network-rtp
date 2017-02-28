module Data.MediaBus.RtpSpec ( spec ) where

import           Conduit
import           Data.Conduit.List
import           Data.MediaBus
import           Test.Hspec
import qualified Data.MediaBus.Rtp.Packet       as Rtp
import qualified Data.ByteString                as B
import           Data.Word
import           Control.Lens
import           Data.Proxy

spec :: Spec
spec = rtpSourceSpec >> rtpPayloadDemuxSpec

rtpSourceSpec :: Spec
rtpSourceSpec = describe "rtpSource" $ do
    let countStarts :: [Series a b] -> Int
        countStarts = foldr (\x n -> case x of
                                 Start _ -> n + 1
                                 Next _ -> n)
                            0
        runTestConduit inputs = runConduitPure (sourceList inputs .|
                                                    annotateTypeCIn (Proxy :: Proxy (Stream Int Int Int () B.ByteString))
                                                                    rtpSource .|
                                                    consume)

    it "yields 'Start' when only when the first payload packet arrives" $
        let inputs = [ MkStream (Start (MkFrameCtx 0 0 0 ())) ]
        in
            countStarts (_stream <$> runTestConduit inputs) `shouldBe`
                0

    it "yields 'Start' when the first packet arrives" $
        let inputs = [ MkStream (Start (MkFrameCtx 0 0 0 ()))
                     , mkTestRtpPacket 0 0 0
                     ]
        in
            countStarts (_stream <$> runTestConduit inputs) `shouldBe`
                1

    it "yields 'Start' when the ssrc changes" $
        let inputs = [ MkStream (Start (MkFrameCtx 8 9 10 ()))
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
        let inputs = [ MkStream (Start (MkFrameCtx 0 0 0 ()))
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
        let inputs = [ MkStream (Start (MkFrameCtx 0 0 0 ()))
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
        let inputs = [ MkStream (Start (MkFrameCtx 0 0 0 ()))
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
        let inputs = [ MkStream (Start (MkFrameCtx 0 0 0 ()))
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
        let inputs = [ MkStream (Start (MkFrameCtx 0 0 0 ()))
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

rtpPayloadDemuxSpec :: Spec
rtpPayloadDemuxSpec = describe "rtpPayloadDemux" $ do
    let runTestConduit inputs payloadHandlers fallback =
            runConduitPure (sourceList inputs .|
                                rtpSource .|
                                rtpPayloadDemux payloadHandlers fallback .|
                                consume)

    it "always yields the fallback element if the payload table contains no handler" $
        let inputs = [ MkStream (Start (MkFrameCtx 0 0 0 ()))
                     , mkTestRtpPacket 0 0 0
                     ]
            outs = preview payload <$> runTestConduit inputs [] ()
        in
            outs `shouldBe` [ Nothing, Just () ]
    it "always yields the fallback element if the payload table contains no handler for the payload type" $
        let inputs = MkStream (Start (MkFrameCtx 0 0 0 ())) :
                [ mkTestRtpPacketWithPayload 0
                                             0
                                             0
                                             (mkTestPayload (Rtp.MkRtpPayloadType p))
                | p <- [0 .. 128] ]
            fallback :: SampleBuffer (S16 8000)
            fallback = mempty
            outs = preview payload <$> runTestConduit inputs
                                                      [ ( Rtp.MkRtpPayloadType 129
                                                        , transcode .
                                                            alawPayloadHandler
                                                        )
                                                      ]
                                                      fallback
        in
            outs `shouldBe` Nothing :
                [ Just fallback
                | _ <- [0 .. 128 :: Word8] ]
    it "invokes the first matching payload handler" $
        let inputs = MkStream (Start (MkFrameCtx 0 0 0 ())) :
                [ mkTestRtpPacketWithPayload 0 0 0 (mkTestPayload 8)
                , mkTestRtpPacketWithPayload 0 0 0 (mkTestPayload 0)
                ]
            outputs = preview payload <$> runTestConduit inputs
                                                         [ ( 8
                                                           , payload .~
                                                               "first 8 handler"
                                                           )
                                                         , ( 8
                                                           , payload .~
                                                               "second 8 handler"
                                                           )
                                                         , ( 0
                                                           , payload .~
                                                               "first 0 handler"
                                                           )
                                                         , ( 0
                                                           , payload .~
                                                               "second 0 handler"
                                                           )
                                                         ]
                                                         "bad"
        in
            outputs `shouldBe`
                [ Nothing, Just "first 8 handler", Just "first 0 handler" ]

mkBrokenTestRtpPacket :: Stream Int Int Int () B.ByteString
mkBrokenTestRtpPacket = MkStream (Next (MkFrame 0 0 (B.pack [ 0, 0, 0 ])))

mkTestPayload :: Rtp.RtpPayloadType -> Rtp.RtpPayload
mkTestPayload pt = Rtp.MkRtpPayload pt (sampleBufferFromList [ 0, 0, 0 ])

mkTestRtpPacket :: Rtp.RtpSsrc
                -> Rtp.RtpSeqNum
                -> Rtp.RtpTimestamp
                -> Stream Int Int Int () B.ByteString
mkTestRtpPacket ssrc sn ts =
    mkTestRtpPacketWithPayload ssrc sn ts (mkTestPayload 0)

mkTestRtpPacketWithPayload :: Rtp.RtpSsrc
                           -> Rtp.RtpSeqNum
                           -> Rtp.RtpTimestamp
                           -> Rtp.RtpPayload
                           -> Stream Int Int Int () B.ByteString
mkTestRtpPacketWithPayload ssrc sn ts p =
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
                                                            p))))
