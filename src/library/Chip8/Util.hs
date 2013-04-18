module Chip8.Util where

import Data.Bits   ((.&.), shiftR, testBit)
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

toBoolList :: Word8 -> [Bool]
toBoolList w = reverse . toBoolList' w $ 0
  where
    toBoolList' _ 8 = []
    toBoolList' w b = testBit w b : toBoolList' w (b + 1)
