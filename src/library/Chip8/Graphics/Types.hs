module Chip8.Graphics.Types where

import Data.Array.BitArray
import Data.Array.BitArray.IO

type VideoMemory = IOBitArray Int

vScale = 8
vWidth  = 64
vHeight = 32

newVideoMemory :: Integer -> IO VideoMemory
newVideoMemory scale = newArray (0, (vScale * vWidth * vScale * vHeight) - 1) False

