module Chip8.Util where

import Data.Bits   ((.&.), shiftR)
import Data.Word   (Word16, Word8)
import Text.Printf (printf)

prettifyWord8 :: Word8 -> String
prettifyWord8 = printf "%02x"

prettifyWord16 :: Word16 -> String
prettifyWord16 = printf "%04x"

nibble :: Num a => Int -> Word16 -> a
nibble n w = fromIntegral $ w `shiftR` (n*4) .&. 0xf

lowByte :: Word16 -> Word8
lowByte w  = fromIntegral $ 0xff .&. w

highByte :: Word16 -> Word8
highByte w = fromIntegral $ 0xff .&. w `shiftR` 8
