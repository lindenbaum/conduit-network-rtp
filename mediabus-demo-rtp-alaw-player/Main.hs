module Main where

import           Data.MediaBus.Internal.AsyncConduit
import           Control.Concurrent.Async.Lifted
import           Conduit
import           Data.MediaBus
import           Data.MediaBus.Internal.Conduit
import           Data.MediaBus.Applications.RtpAlawAudio

{- Send test data with:
#!/bin/bash

PORT=${1?port missing}
MY_IP=${2?host ip missing}
FNAME=${3:-28797-04.ogg}
FILE=$(realpath $(dirname ${0})/$FNAME)

gst-launch-1.0  uridecodebin uri=file://$FILE ! audioconvert ! audioresample !  audio/x-raw,format=S16LE,rate=8000,channels=1 ! alawenc ! rtppcmapay pt=8 mtu=172 min-ptime=10000000 max-ptime=200000000  ptime-multiple=5000000 ! udpsink host=$MY_IP port=$PORT
-}
maxFrames :: Int
maxFrames = 15000

main :: IO ()
main = mainSync

mainASync :: IO ()
mainASync = runResourceT $ do
    (asrc, src) <- periodicRtpAlawUdpReceiver16kHzS16OrSilence 10000
                                                               "127.0.01"
                                                               (10 / 1000)
                                                               20
    runConduit (src .| debugExitAfter maxFrames .|
                    streamDebugPlaybackSink)
    cancel asrc

mainASync2 :: IO ()
mainASync2 = do
    let pTime = 10 / 1000
        qlen = 20
        pollIntervall = 0.5 * fromIntegral qlen * pTime
    runResourceT $
        connectConcurrentlyPolledSourceToSink qlen
                                              pollIntervall
                                              pTime
                                              (rtpAlawUdpReceiver16kHzS16 10000
                                                                          "127.0.01"
                                                                          pTime
                                                                          qlen)
                                              (concealMissing blankFor .|
                                                   debugExitAfter maxFrames .|
                                                   streamDebugPlaybackSink)


mainSync :: IO ()
mainSync = runConduitRes (rtpAlawUdpReceiver16kHzS16 10000
                                                            "127.0.01"
                                                            (10 / 1000)
                                                            20 .|
                              debugExitAfter maxFrames .|
                              streamDebugPlaybackSink)
