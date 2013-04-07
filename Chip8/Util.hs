module Chip8.Util where

import Data.Bits   ((.&.), shiftR)
import Data.Word   (Word16, Word8)
import Text.Printf (printf)

prettifyWord16 :: Word16 -> String
prettifyWord16 = printf "%04x"

nibble :: Int -> Word16 -> Word16
nibble n w = w `shiftR` (n*4) .&. 0xf

lowByte :: Word16 -> Word8
lowByte w  = fromIntegral $ 0xff .&. w

highByte :: Word16 -> Word8
highByte w = fromIntegral $ 0xff .&. w `shiftR` 8
