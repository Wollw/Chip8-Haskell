module Chip8.Event where

import Data.Array.IO

import Graphics.UI.SDL as SDL

data Key = K0 | K1 | K2 | K3 | K4 | K5 | K6 | K7
         | K8 | K9 | KA | KB | KC | KD | KE | KF
         deriving (Show, Enum, Ix, Ord, Eq)

type KeyState = IOUArray Key Bool

newKeyState :: IO KeyState
newKeyState = newArray (K0, KF) False

setKey :: KeyState -> Key -> Bool -> IO ()
setKey ks k b = writeArray ks k b

keyDown :: KeyState -> Key -> IO Bool
keyDown ks k = readArray ks k

checkEvents :: KeyState -> IO ()
checkEvents ks = do
    e  <- pollEvent
    case e of
        KeyDown key -> setKeyHandler key True
        KeyUp   key -> setKeyHandler key False
        _           -> return ()
  where
    setKeyHandler (Keysym key _ _) b = do
        let k = case key of
                SDLK_7          -> Just K1
                SDLK_8          -> Just K2
                SDLK_9          -> Just K3
                SDLK_MINUS      -> Just KC
                SDLK_u          -> Just K4
                SDLK_i          -> Just K5
                SDLK_o          -> Just K6
                SDLK_p          -> Just KD
                SDLK_j          -> Just K7
                SDLK_k          -> Just K8
                SDLK_l          -> Just K9
                SDLK_SEMICOLON  -> Just KE
                SDLK_m          -> Just KA
                SDLK_COMMA      -> Just K0
                SDLK_PERIOD     -> Just KB
                SDLK_SLASH      -> Just KF
                _               -> Nothing
        case k of
            Just k -> setKey ks k b
            _      -> return ()
