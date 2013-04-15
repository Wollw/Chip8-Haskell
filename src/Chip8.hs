import Graphics.UI.SDL as SDL

import Chip8.Emulator
import Data.ByteString

runFile fp = do
    SDL.init [InitVideo, InitAudio]
    screen <- setVideoMode (64*8) (32*8) 0 [HWSurface]
    rom <- fmap unpack . Data.ByteString.readFile $ fp
    run screen (IBytes rom)
    return ()
