module Data.MediaBus.RtpSpec ( spec ) where

import           Test.Hspec
import           Test.QuickCheck
import           Data.Conduit
import           Data.Word

import Data.Default
-- import Data.Conduit.Lift
import Control.Monad.State.Strict

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
--   stupid simple SSRC handling
--      --> manage a list of MediaConduits one for each SSRC
--      --> maybe enforce an upper limit on the number of concurrent SSRCs (maybe 2)
--      --> ssrcDemux :: Monad m => (forall s. Default s => Ssrc -> Conduit MediaData (StateT s m) MediaData)
--                               -> Conduit MediaData m MediaData
newtype MediaStreamId = MkMediaStreamId { fromMediaStreamId :: Word32 }
    deriving (Show)

data MediaData = MkMediaData { mediaBuffer :: String }
    deriving (Show)

data MediaStream = MkMediaStream { mediaId      :: MediaStreamId
                                 , streamPacket :: MediaData
                                 }
    deriving (Show)

multiStream :: (Default s, Monad m)
            => (MediaStreamId -> Conduit MediaData (StateT s m) MediaData)
            -> Conduit MediaStream m MediaStream
multiStream = undefined
--      --> Then assume: type MediaFunction = forall m. Monad m => Conduit MediaData m MediaData
--          and also a simple MediaMixer like:


--
--   Packet Loss Detection, Packet Ordering and Jitter
--      -->
--   create/use decoder for specific payload type
--      --> (forall  s t. (Default s, Monad m) => (RtpPayloadType -> Conduit MediaData (StateT s m) MediaData) -> Conduit MediaData m MediaData)
--     aggregate media data in a Set
--        --> Sort by
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
