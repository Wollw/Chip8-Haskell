module Chip8.Graphics where

import Chip8.Memory
import Chip8.Util
import Chip8.Graphics.Types

import Control.Monad

import Data.Array.MArray
import Data.BitArray
import Data.BitArray.IO

import Graphics.UI.SDL as SDL

data PixelState = On | Off
  deriving (Show, Eq)

boolToPixelState :: Bool -> PixelState
boolToPixelState True  = On
boolToPixelState False = Off


drawPixel :: VideoMemory -> Int -> Int -> PixelState -> IO ()
drawPixel vram x y On  = writeBit vram (posIndex x y) True
drawPixel vram x y Off = writeBit vram (posIndex x y) False

drawSprite :: VideoMemory -> Int -> Int -> [PixelState] -> IO Bool
drawSprite vram dx dy ps = do
    (_, erased) <- foldM setPixel (0,False) ps
    return erased
  where
    setPixel (a,e) state = do
        let x = a `mod` 8
        let y = a `div` 8
        currentState <- readBit vram (posIndex (dx + x) (dy + y))
        let e' = if state == On && currentState == True then True else False
        case state == On of
            True  -> drawPixel vram (dx + x) (dy + y) $ if e' then Off else state
            False -> return ()
        return $ (a + 1, if e' || e then True else False)

drawSpriteLocation :: Memory -> VideoMemory -> Int -> Int -> Int -> Address -> IO Bool
drawSpriteLocation mem vram x y n (Ram addr) = do
    sprite <- fmap (take n . drop (fromIntegral addr)) $ getElems (ram mem)
    let bools = concat . map toBoolList $ sprite
    drawSprite vram x y $ map boolToPixelState bools

posIndex x y = (y `mod` vHeight) * vWidth + (x `mod` vWidth)

drawVideoMemory :: Surface -> VideoMemory -> IO ()
drawVideoMemory screen vm = do
    let r = Rect 0 0 (vWidth * vScale) (vHeight * vScale)
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
pixel x y = Rect (vScale * x) (vScale * y) vScale vScale