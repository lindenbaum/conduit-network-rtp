Serialize and deserialize RTP packets.
TODO: Add RTCP support

> module Data.Conduit.Audio.RtpPacket
>   ( Packet(..), Header(..), HeaderExtension(..)
>   , SeqNum(..)
>   , deserialize, serialize)
> where

> import qualified Data.ByteString as B
> import Control.Applicative
> import Control.Monad
> import Data.Monoid
> import Data.Maybe
> import Data.Serialize.Get
> import Data.Serialize.Put
> import Text.Printf
> import Data.Word
> import Data.Bits


The relevant output will be contained in the 'Packet' and 'Header' data types.
The Functor style of 'Packet' allows to keep the RTP header info around, while
converting and analysing the packet.

> data Packet =
>   Packet { header :: !Header
>          , body   :: !B.ByteString }
>   deriving (Eq)


> data Header =
>   Header { version         :: !Word8
>          , hasPadding      :: !Bool
>          , hasMarker       :: !Bool
>          , payloadType     :: !Word8
>          , sequenceNumber  :: !SeqNum
>          , timestamp       :: !Word32
>          , ssrc            :: !Word32
>          , csrcs           :: ![Word32]
>          , headerExtension :: !(Maybe HeaderExtension)}
>   deriving (Eq)

Sequence numbers are special because they wrap-around.

To meaningfully compare them, this must be taken into account.
E.g. when @x1 = 65535@ is the current sequence number and the next
packet has @x2 = 0@ then in this context @x2 > x1@.

This newtype wrapper allows to add a corresponding 'Ord' instance.

> newtype SeqNum = MkSeqNum {unSeqNum :: Word16}
>   deriving (Num, Show)

The 'Eq' instance is straight forward:

> instance Eq SeqNum where
>   (MkSeqNum !seqL) == (MkSeqNum !seqR) = seqL == seqR


This 'Ord' instance will regard @0 <= 1@ and @65535 <= 0@.

> instance Ord SeqNum where
>   (MkSeqNum !seqL) <= (MkSeqNum !seqR) =
>     seqL <= seqR && seqR - seqL < 32768


The 'HeaderExtension' is a profile specific, variable length, data block
following the fixed size RTP header:

> data HeaderExtension =
>   HeaderExtension { headerExtensionField :: !Word16
>                   , headerExtensionBody  :: ![Word32] }
>   deriving (Read,Eq,Show)


Deserialize a complete RTP datagram:

> deserialize :: B.ByteString -> Packet
> deserialize bs = either error id (runGet getPacket bs)

Below are only internal functions.

This function will parse an 'Rtp' packet from a 'ByteString':

> getPacket :: Get Packet
> getPacket = do

First read the header:

>   h <- getHeader

Then read the remaining bytes:

>   remainingLen <- remaining
>   remainingBytes <- getBytes remainingLen

And then adjust for padding:

>   let body = if hasPadding h
>                 then adjustPadding remainingBytes
>                 else remainingBytes

Wrap everything up and return it:

>   return (Packet h body)

Ok now to adjust for padding:

> adjustPadding :: B.ByteString -> B.ByteString
> adjustPadding padded =
>   let paddingLen = B.last padded
>       unpaddedLen = B.length padded - fromIntegral paddingLen
>       in B.take unpaddedLen padded

This function will parse an 'RtpHeader':

> getHeader :: Get Header
> getHeader = do

The values are in network byte order, i.e. big-endian.

>   b1 <- getWord8

From https://www.ietf.org/rfc/rfc3550.txt, section 5:

 The RTP header has the following format:
  0                   1                   2                   3
  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 |V=2|P|X|  CC   |M|     PT      |       sequence number         |
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 |                           timestamp                           |
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 |           synchronization source (SSRC) identifier            |
 +=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
 |            contributing source (CSRC) identifiers             |
 |                             ....                              |
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+


   The first twelve octets are present in every RTP packet, while the
   list of CSRC identifiers is present only when inserted by a mixer.
   The fields have the following meaning:

   version (V): 2 bits
      This field identifies the version of RTP.  The version defined by
      this specification is two (2).  (The value 1 is used by the first
      draft version of RTP and the value 0 is used by the protocol
      initially implemented in the "vat" audio tool.)

>   let version' = fromIntegral ((b1 `shiftR` 6) .&. 3)

   padding (P): 1 bit
      If the padding bit is set, the packet contains one or more
      additional padding octets at the end which are not part of the
      payload.  The last octet of the padding contains a count of how
      many padding octets should be ignored, including itself.  Padding
      may be needed by some encryption algorithms with fixed block sizes
      or for carrying several RTP packets in a lower-layer protocol data
      unit.

>   let hasPadding' = testBit b1 5

   extension (X): 1 bit
      If the extension bit is set, the fixed header MUST be followed by
      exactly one header extension, with a format defined in Section
      5.3.1.

>   let hasExtension = testBit b1 4

   CSRC count (CC): 4 bits
      The CSRC count contains the number of CSRC identifiers that follow
      the fixed header.

>   let csrcCount = b1 .&. 0xf

   marker (M): 1 bit
      The interpretation of the marker is defined by a profile.  It is
      intended to allow significant events such as frame boundaries to
      be marked in the packet stream.  A profile MAY define additional
      marker bits or specify that there is no marker bit by changing the
      number of bits in the payload type field (see Section 5.3).

>   b2 <- getWord8
>   let hasMarker' = testBit b2 7

   payload type (PT): 7 bits
      This field identifies the format of the RTP payload and determines
      its interpretation by the application.  A profile MAY specify a
      default static mapping of payload type codes to payload formats.
      Additional payload type codes MAY be defined dynamically through
      non-RTP means (see Section 3).  A set of default mappings for
      audio and video is specified in the companion RFC 3551 [1].  An
      RTP source MAY change the payload type during a session, but this
      field SHOULD NOT be used for multiplexing separate media streams
      (see Section 5.2).

      A receiver MUST ignore packets with payload types that it does not
      understand.

>   let payloadType' = fromIntegral (b2 .&. 0x7f)

   sequence number: 16 bits
      The sequence number increments by one for each RTP data packet
      sent, and may be used by the receiver to detect packet loss and to
      restore packet sequence.  The initial value of the sequence number
      SHOULD be random (unpredictable) to make known-plaintext attacks
      on encryption more difficult, even if the source itself does not
      encrypt according to the method in Section 9.1, because the
      packets may flow through a translator that does.  Techniques for
      choosing unpredictable numbers are discussed in [17].

>   sequenceNumber' <- getWord16be

   timestamp: 32 bits
      The timestamp reflects the sampling instant of the first octet in
      the RTP data packet.  The sampling instant MUST be derived from a
      clock that increments monotonically and linearly in time to allow
      synchronization and jitter calculations (see Section 6.4.1).  The
      resolution of the clock MUST be sufficient for the desired
      synchronization accuracy and for measuring packet arrival jitter
      (one tick per video frame is typically not sufficient).  The clock
      frequency is dependent on the format of data carried as payload
      and is specified statically in the profile or payload format
      specification that defines the format, or MAY be specified
      dynamically for payload formats defined through non-RTP means.  If
      RTP packets are generated periodically, the nominal sampling
      instant as determined from the sampling clock is to be used, not a
      reading of the system clock.  As an example, for fixed-rate audio
      the timestamp clock would likely increment by one for each
      sampling period.  If an audio application reads blocks covering
      160 sampling periods from the input device, the timestamp would be
      increased by 160 for each such block, regardless of whether the
      block is transmitted in a packet or dropped as silent.

      The initial value of the timestamp SHOULD be random, as for the
      sequence number.  Several consecutive RTP packets will have equal
      timestamps if they are (logically) generated at once, e.g., belong
      to the same video frame.  Consecutive RTP packets MAY contain
      timestamps that are not monotonic if the data is not transmitted
      in the order it was sampled, as in the case of MPEG interpolated
      video frames.  (The sequence numbers of the packets as transmitted
      will still be monotonic.)

      RTP timestamps from different media streams may advance at
      different rates and usually have independent, random offsets.
      Therefore, although these timestamps are sufficient to reconstruct
      the timing of a single stream, directly comparing RTP timestamps
      from different media is not effective for synchronization.
      Instead, for each medium the RTP timestamp is related to the
      sampling instant by pairing it with a timestamp from a reference
      clock (wallclock) that represents the time when the data
      corresponding to the RTP timestamp was sampled.  The reference
      clock is shared by all media to be synchronized.  The timestamp
      pairs are not transmitted in every data packet, but at a lower
      rate in RTCP SR packets as described in Section 6.4.

      The sampling instant is chosen as the point of reference for the
      RTP timestamp because it is known to the transmitting endpoint and
      has a common definition for all media, independent of encoding
      delays or other processing.  The purpose is to allow synchronized
      presentation of all media sampled at the same time.

      Applications transmitting stored data rather than data sampled in
      real time typically use a virtual presentation timeline derived
      from wallclock time to determine when the next frame or other unit
      of each medium in the stored data should be presented.  In this
      case, the RTP timestamp would reflect the presentation time for
      each unit.  That is, the RTP timestamp for each unit would be
      related to the wallclock time at which the unit becomes current on
      the virtual presentation timeline.  Actual presentation occurs
      some time later as determined by the receiver.

      An example describing live audio narration of prerecorded video
      illustrates the significance of choosing the sampling instant as
      the reference point.  In this scenario, the video would be
      presented locally for the narrator to view and would be
      simultaneously transmitted using RTP.  The "sampling instant" of a
      video frame transmitted in RTP would be established by referencing
      its timestamp to the wallclock time when that video frame was
      presented to the narrator.  The sampling instant for the audio RTP
      packets containing the narrator's speech would be established by
      referencing the same wallclock time when the audio was sampled.
      The audio and video may even be transmitted by different hosts if
      the reference clocks on the two hosts are synchronized by some
      means such as NTP.  A receiver can then synchronize presentation
      of the audio and video packets by relating their RTP timestamps
      using the timestamp pairs in RTCP SR packets.

>   timestamp' <- getWord32be

   SSRC: 32 bits
      The SSRC field identifies the synchronization source.  This
      identifier SHOULD be chosen randomly, with the intent that no two
      synchronization sources within the same RTP session will have the
      same SSRC identifier.  An example algorithm for generating a
      random identifier is presented in Appendix A.6.  Although the
      probability of multiple sources choosing the same identifier is
      low, all RTP implementations must be prepared to detect and
      resolve collisions.  Section 8 describes the probability of
      collision along with a mechanism for resolving collisions and
      detecting RTP-level forwarding loops based on the uniqueness of
      the SSRC identifier.  If a source changes its source transport
      address, it must also choose a new SSRC identifier to avoid being
      interpreted as a looped source (see Section 8.2).

>   ssrc' <- getWord32be

   CSRC list: 0 to 15 items, 32 bits each
      The CSRC list identifies the contributing sources for the payload
      contained in this packet.  The number of identifiers is given by
      the CC field.  If there are more than 15 contributing sources,
      only 15 can be identified.  CSRC identifiers are inserted by
      mixers (see Section 7.1), using the SSRC identifiers of
      contributing sources.  For example, for audio packets the SSRC
      identifiers of all sources that were mixed together to create a
      packet are listed, allowing correct talker indication at the
      receiver.

>   csrcs' <- sequence (replicate (fromIntegral csrcCount) getWord32be)

If a the extension flag is set, we must parse an optional header extension:

>   extension  <- if hasExtension
>                    then Just <$> getHeaderExtension
>                    else return Nothing

>   return (Header
>             { version = version'
>             , hasPadding = hasPadding'
>             , hasMarker = hasMarker'
>             , payloadType = payloadType'
>             , sequenceNumber = MkSeqNum sequenceNumber'
>             , timestamp = timestamp'
>             , ssrc = ssrc'
>             , csrcs = csrcs'
>             , headerExtension = extension })


The RFC defines how RTP header extensions must be provided.

Quoting the RFC again:

    0                   1                   2                   3
    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |      defined by profile       |           length              |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                        header extension                       |
   |                             ....                              |

   If the X bit in the RTP header is one, a variable-length header
   extension MUST be appended to the RTP header, following the CSRC list
   if present.  The header extension contains a 16-bit length field that
   counts the number of 32-bit words in the extension, excluding the
   four-octet extension header (therefore zero is a valid length).  Only
   a single extension can be appended to the RTP data header.

> getHeaderExtension :: Get HeaderExtension
> getHeaderExtension = do
>   field  <- getWord16be
>   len    <- getWord16be
>   body   <- sequence (replicate (fromIntegral len) getWord32be)
>   return (HeaderExtension field body)

NOTE: To test this, you can use gstreamer, e.g. with this command line:

@@@
gst-launch-1.0 autoaudiosrc is-live=true ! audioconvert ! audioresample ! alawenc ! rtppcmapay pt=8 mtu=172 ssrc=123345 ! udpsink host=someip port=theport
@@@

Here are the type class instances:

> instance Show Packet where
>   show (Packet hdr bd) = printf "RTP %s << " (show hdr)
>     (printf "Pcm buffer with %d samples:\n<< " (B.length bd)
>       ++
>       (if B.length bd > 10
>        then unwords (printf "%0.4x" <$> B.unpack (B.take 10 bd)) ++ " ..."
>        else unwords (printf "%0.4x" <$> B.unpack bd))
>       ++  " >>\n")


A monoid instance can be nice:

> instance Monoid Packet where
>   mempty = Packet mempty mempty
>   mappend (Packet h1 b1) (Packet h2 b2) = Packet (h1 <> h2) (b1 <> b2)

Of course, the interesting things happening in 'Header's instance:

> instance Monoid Header where
>   mempty = Header 2 False False 0 0 0 0 [] Nothing
>   mappend (Header v1 p1 m1 pt1 seq1 ts1 ssrc1 csrcs1 hes1)
>           (Header v2 p2 m2 pt2 seq2 ts2 ssrc2 csrcs2 hes2) =
>     Header
>          (max v1 v2)
>          (p1 || p2)
>          (m1 || m2)
>          (if pt1 == 0
>             then pt2
>             else
>               if pt2 == 0
>                 then pt1
>                 else
>                   if pt1 /= pt2
>                      then error "payload type mismatch"
>                      else pt1)
>          (if seq1 == 0
>             then seq2
>             else
>               if seq2 == 0
>                 then seq1
>                 else min seq1 seq2)
>          (if ts1 == 0
>             then ts2
>             else
>               if ts2 == 0
>                 then ts1
>                 else min ts1 ts2)
>          (if ssrc1 == 0
>             then ssrc2
>             else
>               if ssrc2 == 0
>                 then ssrc1
>                 else
>                   if ssrc1 /= ssrc2
>                      then error "ssrc mismatch"
>                      else ssrc1)
>          (csrcs1 <> csrcs2)
>          (hes1 <|> hes2)

> instance Show Header where
>   show (Header _ _ m pt s ts ssrc _csrcs hes) =
>     printf "ssrc:%09d/pt:%d/ts:%09d/seq:%05d/m:%d/e:%s"
>           ssrc
>           pt
>           ts
>           (unSeqNum s)
>           (if m then 0 else 1::Int)
>           (maybe "0" (("|" ++) . show) hes)

Serialization is straight forward the opposite of deserialization.

> serialize :: Packet -> B.ByteString
> serialize pkg = runPut (putPacket pkg)

> putPacket :: Packet -> Put
> putPacket (Packet h b) = do

First write the header then the body.

>   putHeader h
>   putByteString b

Calculate number of bytes required for padding.

>   let paddingLen = fromIntegral ((64 - ((B.length b) `rem` 64)) `rem` 64)

The 'Header' field 'hasPadding', which is an input to this function,
is interpreted to indicate if padding is /allowed/.

>   when (hasPadding h && paddingLen > 0) $ do

Generate the padding. The last byte of the padding must contain the padding
length. The contents of the (other) padding bytes are ignored, so we can use
a little Haskell trick to generate a list of bytes with the last containing the
number of bytes in that list.

>     let padding = [1 .. paddingLen]
>     mapM_ putWord8 padding

Writing out the header:

> putHeader :: Header -> Put
> putHeader Header{..} = do

To repeat the RTP header structure:

  0                   1                   2                   3
  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 |V=2|P|X|  CC   |M|     PT      |       sequence number         |
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 |                           timestamp                           |
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 |           synchronization source (SSRC) identifier            |
 +=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
 |            contributing source (CSRC) identifiers             |
 |                             ....                              |
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

The first byte is the hardest:

>   putWord8

Set the extension header flag

>     (setBitTo 4 (isJust headerExtension)

Set the padding flag:

>      (setBitTo 5 hasPadding

Set the version:

>      (shiftL version 6  .|.

Write the number of CSRCs:

>      (fromIntegral (length csrcs) .&. 0xf))))

The second byte contains the marker and the payload type:

>   putWord8
>     (setBitTo 7 hasMarker
>      (payloadType .&. 0x7f))

The sequence number and timestamp:

>   putWord16be (unSeqNum sequenceNumber)
>   putWord32be timestamp

The SSRC:

>   putWord32be ssrc

The maximum of 16 csrcs:

>   mapM_ putWord32be (take 16 csrcs)

And last but not least the header extensions:

>   mapM_ putHeaderExtension headerExtension

Serialize the 'HeaderExtension':

> putHeaderExtension :: HeaderExtension -> Put
> putHeaderExtension HeaderExtension{..} = do
>   putWord16be headerExtensionField

Write the number of 'Word32's that make up the header extension body. Limit the
length to 0xffff since the length field is only 16 bits wide.

>   let bodyLen = min 0xffff (length headerExtensionBody)
>   putWord16be (fromIntegral bodyLen)
>   mapM_ putWord32be (take bodyLen headerExtensionBody)

A litte binary helper:

> setBitTo :: Bits a => Int -> Bool -> a -> a
> setBitTo ix cond a =
>   (if cond then setBit else clearBit) a ix
