module Data.MediaBus.RtpSpec ( spec ) where

import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
    describe "incoming packet handling" $ do
        describe "packets with unknown SSRC" $ do
            it "just pushes packets to the end of the ring" $
                property $
                    True
            it "rembers the unknown SSRC as the next known SSRC" $
                property $
                    True
        describe "packets with known SSRC" $ do
            it "drops packets that are too old" $
                property $
                    True
            it "stores packets into the ring at the position given through the sequence number" $
                property $ True
    describe "pulling packets" $ do
        it "returns all packets" $ property $ True
        it "fills the gaps with packet loss indications" $ property $ True

-- =================================
-- Network Driven Rtp:
-- per received network packet:
--   Assign NTP timestamp
--   demultiplex sources based on SSRC
--   per SSRC:
--     create/use decoder for specific payload type
--     aggregate media data in ring
-- =================================

-- =================================
-- Periodic Conduit over a Monadic action (the above for instance)
--   sleep for some time
--   initialize the start timestamp map
--   copy all elements from the ring into the aggregation buffer
--      if an element is "old" or missing
--           then remember the current start position in the gap list
--           else convert the start
-- ---------------------------------

-- =================================
-- Re-framer:
--  reframer :: Source m o -> (o -> m (Reframed o'))

-- A downstream event listener
--  encode
--  aggregate
--  frame/multiplex
