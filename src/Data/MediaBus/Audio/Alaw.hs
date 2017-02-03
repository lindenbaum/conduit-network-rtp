module Data.MediaBus.Audio.Alaw
    ( ALaw(..)
    , alawSample
    ) where

import           Foreign.Storable
import           Data.MediaBus.Frame
import           Data.MediaBus.Audio.Raw
import           Data.MediaBus.Audio.Channels
import           Data.Bits
import           Data.Word
import           Data.Int
import           Control.Lens

newtype ALaw = MkALaw { _alawSample :: Word8 }
    deriving (Show, Storable, Num, Eq, Bits)

makeLenses ''ALaw

instance HasChannelLayout ALaw where
    channelLayout _ = SingleChannel

instance Transcoder ALaw S16 clock where
    transcode = sampleFilter (MkS16 . decodeAlawFrame . _alawSample)

instance Transcoder S16 ALaw clock where
    transcode = sampleFilter (MkALaw . encodeAlawFrame . _s16Sample)

decodeAlawFrame :: Word8 -> Int16
decodeAlawFrame !a' = let !a = a' `xor` 85
                          !quant_mask = 15
                          !quant_shift = 4
                          !seg_mask = 112
                          !seg_shift = 4
                          tBase, tAbs, seg :: Int16
                          !seg = (fromIntegral a .&. seg_mask) `shiftR`
                              seg_shift
                          !tBase = (fromIntegral a .&. quant_mask) `shiftL`
                              quant_shift
                          !tAbs = case seg of
                              0 -> tBase + 8
                              1 -> tBase + 264
                              _ -> (tBase + 264) `shiftL`
                                  fromIntegral (seg - 1)
                          !isPos = testBit a 7
                      in
                          if isPos then tAbs else tAbs * (-1)

-- | See http://opensource.apple.com//source/tcl/tcl-20/tcl_ext/snack/snack/generic/g711.c
--
-- >	Linear Input Code	 Compressed Code
-- >	-----------------  ---------------
-- >	  0000000wxyza         000wxyz
-- >	  0000001wxyza         001wxyz
-- >	  000001wxyzab         010wxyz
-- >	  00001wxyzabc         011wxyz
-- >	  0001wxyzabcd         100wxyz
-- >	  001wxyzabcde         101wxyz
-- >	  01wxyzabcdef         110wxyz
-- >	  1wxyzabcdefg         111wxyz
--
-- For further information see John C. Bellamy's Digital Telephony, 1982, John
-- Wiley & Sons, pps 98-111 and 472-476.
encodeAlawFrame :: Int16 -> Word8
encodeAlawFrame !pcmVal' =
    let !pcmVal = pcmVal' `shiftR` 3 -- to 13 bit
        (!mask, !pcmValAbs) = if pcmVal >= 0
                              then ( 0xD5 -- sign (7th) bit = 1
                                   , pcmVal
                                   )
                              else ( 0x55 -- sign bit = 0
                                   , (-1) * pcmVal - 1
                                   )
        -- !segments = [0x1F,0x3F,0x7F,0xFF,0x1FF,0x3FF,0x7FF,0xFFF] :: [
        !segment = if pcmValAbs <= 0x1F
                   then 0
                   else if pcmValAbs <= 0x3F
                        then 1
                        else if pcmValAbs <= 0x7F
                             then 2
                             else if pcmValAbs <= 0xFF
                                  then 3
                                  else if pcmValAbs <= 0x1FF
                                       then 4
                                       else if pcmValAbs <= 0x3FF
                                            then 5
                                            else if pcmValAbs <= 0x7FF
                                                 then 6
                                                 else if pcmValAbs <= 0xFFF
                                                      then 7
                                                      else 8
        !res = if segment == 8
               then 0x7F
               else let !segShift = if segment < 2
                                    then 1
                                    else fromIntegral segment
                    in
                        (shiftL segment 4) .|.
                            (shiftR pcmValAbs segShift .&. 0xF)
    in
        fromIntegral res `xor` mask
