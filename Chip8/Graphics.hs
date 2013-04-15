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

drawPixel :: VideoMemory -> Int -> Int -> PixelState -> IO ()
drawPixel vram x y ON  = writeBit vram (posIndex x y) True
drawPixel vram x y OFF = writeBit vram (posIndex x y) False

drawSprite :: VideoMemory -> Int -> Int -> [PixelState] -> IO ()
drawSprite vram dx dy ps = do
    foldM_ setPixel 0 ps
    return ()
  where
    setPixel a state = do
        let x = a `mod` 8
        let y = a `div` 8
        drawPixel vram (dx + x) (dy + y) state
        return $ a + 1

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
