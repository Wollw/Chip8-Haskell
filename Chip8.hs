import Graphics.UI.SDL as SDL

scale  = 8
width  = 64 * scale
height = 42 * scale

data Keys = Keys {
    up    :: Bool,
    down  :: Bool,
    left  :: Bool,
    right :: Bool
    } deriving (Show)

main = do
    SDL.init [InitVideo, InitAudio]
    fb <- setVideoMode width height 8 [HWSurface]
    return ()
