module Main where

import           Data.MediaBus.AsyncConduit
import           Control.Concurrent.Async.Lifted
import           Conduit
import           Data.MediaBus
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
main = mainASync

mainASync :: IO ()
mainASync = runResourceT $
    withAsyncPolledSource 20
                          (10 / 1000)
                          (rtpAlawUdpReceiver16kHzS16 10000
                                                      "127.0.01"
                                                      (10 / 1000)
                                                      5)
                          (\(_, src) -> runConduit (src .|
                                                        debugExitAfter maxFrames .|
                                                        concealMissing blankFor .|
                                                        streamDebugPlaybackSink))

mainSync :: IO ()
mainSync = runConduitRes (rtpAlawUdpReceiver16kHzS16 10000
                                                     "127.0.01"
                                                     (10 / 1000)
                                                     20 .|
                              debugExitAfter maxFrames .|
                              streamDebugPlaybackSink)
