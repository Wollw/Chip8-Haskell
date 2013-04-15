module Chip8 where

import Graphics.UI.SDL as SDL

import Chip8.Emulator
import Data.ByteString

runFile fp = do
    SDL.init [InitVideo, InitAudio]
    rom <- fmap unpack . Data.ByteString.readFile $ fp
    run (IBytes rom)
    return ()

runFileP fp = do
    SDL.init [InitVideo, InitAudio]
    rom <- fmap unpack . Data.ByteString.readFile $ fp
    runP (IBytes rom)
    return ()
