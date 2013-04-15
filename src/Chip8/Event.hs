module Chip8.Event where

import Data.Array.IO
import Data.IORef

import Graphics.UI.SDL as SDL

data Key = K0 | K1 | K2 | K3 | K4 | K5 | K6 | K7
         | K8 | K9 | KA | KB | KC | KD | KE | KF
         deriving (Show, Enum, Ix, Ord, Eq)

type KeyState = IOUArray Key Bool

data EventState =
    EventState { lastEventKeyDown :: IORef (Maybe Key)
               , keyState    :: KeyState
               }

newEventState :: IO EventState
newEventState = do
    lastEventKeyDown' <- newIORef Nothing
    keyState'     <- newKeyState
    return EventState
        { lastEventKeyDown = lastEventKeyDown'
        , keyState     = keyState'
        }

newKeyState :: IO KeyState
newKeyState = newArray (K0, KF) False

setKey :: KeyState -> Key -> Bool -> IO ()
setKey ks k b = writeArray ks k b

keyDown :: EventState -> Key -> IO Bool
keyDown es k = readArray (keyState es) k

checkEvents :: EventState -> IO ()
checkEvents es = do
    let ks = keyState es
    e  <- pollEvent
    case e of
        KeyDown (Keysym key _ _) -> do
            writeIORef (lastEventKeyDown es) (keyLookup key)
            setKeyHandler key True
        KeyUp   (Keysym key _ _) -> do
            writeIORef (lastEventKeyDown es) Nothing
            setKeyHandler key False
        _ -> do
            writeIORef (lastEventKeyDown es) Nothing
            return ()
  where
    setKeyHandler key b = do
        case keyLookup key of
            Just k -> setKey (keyState es) k b
            _      -> return ()
    keyLookup key = case key of
                SDLK_7          -> Just K1
                SDLK_8          -> Just K2
                SDLK_9          -> Just K3
                SDLK_0          -> Just KC
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
