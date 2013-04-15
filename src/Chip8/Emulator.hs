module Chip8.Emulator where

import Control.Monad.STM
import Control.Concurrent
import Control.Monad
import Control.Monad.Random

import Chip8.Instruction
import Chip8.Memory
import Chip8.Event
import Chip8.Graphics

import Data.Bits
import Data.Word
import Data.Array
import Data.IORef

import Graphics.UI.SDL

data InstructionList
    = IBytes [Word8]
    | ITypes [Instruction]

run :: Surface -> InstructionList -> IO ()
run screen (IBytes rom) = do
    mem  <- newMemory rom
    run' mem
  where
    run' mem = do
        checkEvents (eventstate mem)
        execute mem
        drawVideoMemory screen (vram mem)
        run' mem

runP :: Surface -> InstructionList -> IO ()
runP screen (IBytes rom) = do
    mem  <- newMemory rom
    run' mem
  where
    run' mem = do
        checkEvents (eventstate mem)
        executeP mem
        drawVideoMemory screen (vram mem)
        run' mem

fetchNextInstruction :: Memory -> IO Word16
fetchNextInstruction mem = do
    (Mem16 pc)   <- load mem Pc
    lbyte <- fmap fromIntegral $ loadInt mem (Ram $ pc + 1)
    hbyte <- fmap fromIntegral $ loadInt mem (Ram $ pc)
    return $ (hbyte `shiftL` 8) + lbyte

execute :: Memory -> IO ()
execute m = do
    i <- fmap decodeInstruction $ fetchNextInstruction m
    execute' m i
    case i of      -- Don't increment program counter for jumps.
        (JP _)     -> return ()
        (CALL _)   -> return ()
        (LONGJP _) -> return ()
        _          -> incrementProgramCounter m

executeP :: Memory -> IO ()
executeP m = do
    execute m
    putStrLn ""
    printMemory m

execute' :: Memory -> Instruction -> IO ()
execute' m (SYS addr)       = return () -- Jump to Machine Code, unused
execute' m CLS              = return () -- todo
execute' m RET              = do
    addr <- popStack m
    store m Pc (toMem16 addr)
execute' m (JP (Ram addr))  = store m Pc (toMem16 addr)
execute' m (CALL (Ram adr)) = do
    (Mem16 currentAddr) <- load m Pc
    pushStack m currentAddr
    store m Pc (toMem16 adr)
execute' m (SEByte  vx w)   = do
    x <- loadInt m (Register vx)
    case fromIntegral w == x of
        True -> incrementProgramCounter m
        False -> return ()
execute' m (SNEByte vx w)   = do
    x <- loadInt m (Register vx)
    case fromIntegral w /= x of
        True -> incrementProgramCounter m
        False -> return ()
execute' m (SEAddr vx vy)  = do
    x <- loadInt m (Register vx)
    y <- loadInt m (Register vy)
    case fromIntegral x == y of
        True -> incrementProgramCounter m
        False -> return ()
execute' m (LDByte  vx w)   = store m (Register vx) (Mem8 w)
execute' m (ADDByte vx w)   = do
    x <- loadInt m (Register vx)
    store m (Register vx) $ toMem8 (fromIntegral w + x)
execute' m (LDReg   vx vy)  = do
    (Mem8 y) <- load m (Register vy)
    store m (Register vx) (Mem8 y)
execute' m (OR      vx vy)  = do
    x <- loadInt m (Register vx)
    y <- loadInt m (Register vy)
    store m (Register vx) (toMem8 $ x .|. y)
execute' m (AND     vx vy)  = do
    x <- loadInt m (Register vx)
    y <- loadInt m (Register vy)
    store m (Register vx) (toMem8 $ x .&. y)
execute' m (XOR     vx vy)  = do
    x <- loadInt m (Register vx)
    y <- loadInt m (Register vy)
    store m (Register vx) (toMem8 $ x `xor` y)
execute' m (ADDReg  vx vy)  = do
    x <- loadInt m (Register vx)
    y <- loadInt m (Register vy)
    store m (Register VF) (toMem8 $ if x + y > 255 then 1 else 0)
    store m (Register vx) (toMem8 $ x + y)
execute' m (SUB vx vy)      = do
    x <- loadInt m (Register vx)
    y <- loadInt m (Register vy)
    store m (Register VF) (toMem8 $ if x > y then 1 else 0)
    store m (Register vx) (toMem8 $ x - y)
execute' m (SHR vx)         = do
    x <- loadInt m (Register vx)
    store m (Register VF) (toMem8 $ x .&. 0x01)
    store m (Register vx) (toMem8 $ x `shiftR` 1)
execute' m (SUBN vx vy)     = do
    x <- loadInt m (Register vx)
    y <- loadInt m (Register vy)
    store m (Register VF) (toMem8 $ if x < y then 1 else 0)
    store m (Register vx) (toMem8 $ y - x)
execute' m (SHL vx)         = do
    x <- loadInt m (Register vx)
    store m (Register VF) (toMem8 $ (x .&. 0x80) `shiftR` 7)
    store m (Register vx) (toMem8 $ x `shiftL` 1)
execute' m (SNEAddr vx vy)  = do
    x <- loadInt m (Register vx)
    y <- loadInt m (Register vy)
    case fromIntegral x /= y of
        True -> incrementProgramCounter m
        False -> return ()
execute' m (LDI (Ram addr)) = do
    store m (Register I) (toMem16 addr)
execute' m (LONGJP (Ram a)) = do
    x <- loadInt m (Register V0)
    store m Pc (toMem16 $ fromIntegral a + x)
execute' m (RND vx w) = do
    r <- getRandomR (0,255) :: IO Word8
    store m (Register vx) (toMem8 $ w .&. r)
execute' m (DRW vx vy n) = do
    x <- loadInt m (Register vx)
    y <- loadInt m (Register vy)
    addr <- loadInt m (Register I)
    flip <- drawSpriteLocation m (vram m) x y (fromIntegral n) (Ram $ fromIntegral addr)
    store m (Register VF) (toMem8 $ if flip then 1 else 0)
    return ()
execute' m (SKP vx)    = do
    k <- fmap toEnum $ loadInt m (Register V0)
    pressed <- keyDown (eventstate m) k
    case pressed of
        True  -> incrementProgramCounter m
        False -> return ()
execute' m (SKNP vx)   = do
    k <- fmap toEnum $ loadInt m (Register V0)
    pressed <- keyDown (eventstate m) k
    case pressed of
        True  -> return ()
        False -> incrementProgramCounter m
execute' m (LDVxDT vx) = do
    return () -- todo
execute' m (LDKey vx)  = do
    k <- fmap fromEnum $ getKey
    store m (Register vx) $ (toMem8 . fromEnum) k
    return ()
  where
    getKey = do
        checkEvents (eventstate m)
        k <- readIORef $ lastEventKeyDown (eventstate m)
        case k of
            Just k  -> return k
            Nothing -> getKey
execute' m (LDDTVx vx) = do
    return () -- todo
execute' m (LDSTVx vx) = do
    return () -- todo
execute' m (ADDI vx)   = do
    i <- loadInt m (Register I)
    x <- loadInt m (Register vx)
    store m (Register I) $ toMem16 (i + x)
execute' m (LDF vx)    = do
    x <- fmap (.&.0xf) $ loadInt m (Register vx)
    store m (Register I) $ toMem16 (x * 5)
execute' m (LDB vx)    = do
    x <- loadInt m (Register vx)
    i <- fmap fromIntegral $ loadInt m (Register I)
    foldM_ (\a y -> store m (Ram $ i + a) (toMem8 y) >> return (a + 1)) 0 $ digits x
  where
    digits = reverse . digitsR
    digitsR 0 = []
    digitsR x = x `mod` 10 : digitsR (x `div` 10)
execute' m (LDRegsToI vx) = do
    i <- fmap fromIntegral $ loadInt m (Register I)
    foldM_ (\a v -> do
                x <- load m (Register v)
                store m (Ram $ i + a) x
                return $ a + 1
           ) 0 $ [V0 .. vx]
execute' m (LDRegsFromI vx) = do
    i <- fmap fromIntegral $ loadInt m (Register I)
    foldM_ (\a v -> do
                x <- load m (Ram $ i + a)
                store m (Register v) x
                return $ a + 1
           ) 0 $ [V0 .. vx]

loadInt :: Memory -> Address -> IO Int
loadInt m (Register I) = do
    (Mem16 x) <- load m (Register I)
    return . fromIntegral $ x
loadInt m addr = do
    (Mem8  x) <- load m addr
    return . fromIntegral $ x
