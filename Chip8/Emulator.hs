module Chip8.Emulator where

import Chip8.CPU
import Chip8.Memory

import Data.Bits

run :: [Instruction] -> IO ()
run is = do
    mem <- new
    mapM_ (execute mem) is

runP :: [Instruction] -> IO ()
runP is = do
    mem <- new
    mapM_ (executeP mem) is

execute :: Memory -> Instruction -> IO ()
execute m i = do
    execute' m i
    incPc m
executeP :: Memory -> Instruction -> IO ()
executeP m i = do
    execute m i
    printMemory m

execute' :: Memory -> Instruction -> IO ()
execute' m (SYS a)          = return () -- Jump to Machine Code, unused
execute' m CLS              = return () -- todo
execute' m RET              = return () -- todo
execute' m (JP a)           = return () -- todo
execute' m (CALL a)         = return () -- todo
execute' m (SEByte  vx w)   = do
    x <- loadInt m (Register vx)
    case fromIntegral w == x of
        True -> incPc m
        False -> return ()
execute' m (SNEByte vx w)   = return () -- todo
execute' m (SEAddr  vx vy)  = return () -- todo
execute' m (LDByte  vx w)   = store m (Register vx) (Mem8 w)
execute' m (ADDByte vx w)   = do
    result  <- loadf (w+) m (Register vx)
    store m (Register vx) result
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
execute' m (SNEAddr vx vr)  = return () -- todo

loadf f m r = do
    (Mem8 x) <- load m r
    return (Mem8 $ f x)

loadInt :: Memory -> Address -> IO Int
loadInt m (Register I) = do
    (Mem16 x) <- load m (Register I)
    return . fromIntegral $ x
loadInt m addr = do
    (Mem8  x) <- load m addr
    return . fromIntegral $ x

toMem8  x = Mem8  $ fromIntegral x
toMem16 x = Mem16 $ fromIntegral x

incPc m = do
    (Mem16 pc) <- load m Pc
    store m Pc $ toMem16 (pc + 0x02)
