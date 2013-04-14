module Chip8.Graphics where

import Control.Monad

import Data.BitArray
import Data.BitArray.IO

import Graphics.UI.SDL as SDL

type VideoMemory = IOBitArray

vWidth  = 64
vHeight = 32
scale   = 10

data PixelState = ON | OFF

newVideoMemory :: IO VideoMemory
newVideoMemory = newBitArray (0, vWidth * vHeight) True

setPixel :: VideoMemory -> Int -> Int -> PixelState -> IO ()
setPixel ba x y ON  = writeBit ba (posIndex x y) True
setPixel ba x y OFF = writeBit ba (posIndex x y) False

posIndex x y = y * vWidth + x

drawVideoMemory :: Surface -> VideoMemory -> IO ()
drawVideoMemory screen vm = do
    let r = Rect 0 0 (vWidth * scale) (vHeight * scale)
    ba <- fmap bits . freezeBitArray $ vm
    foldM_ drawBit 0 ba
    SDL.flip screen
  where
    drawBit a i = do
        let x = a `mod` vWidth
        let y = a `div` vWidth
        fillRect screen (Just $ pixel x y) (Pixel $ if i then black else white)
        return $ a + 1

white = 0xffffffff
black = 0x00000000
pixel x y = Rect (scale * x) (scale * y) scale scale
