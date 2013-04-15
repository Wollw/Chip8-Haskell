module Chip8.Graphics.Types where

import Data.BitArray.IO

type VideoMemory = IOBitArray

vScale = 8
vWidth  = 64
vHeight = 32

newVideoMemory :: Integer -> IO VideoMemory
newVideoMemory scale = newBitArray (0, (vScale * vWidth * vScale * vHeight) - 1) False

