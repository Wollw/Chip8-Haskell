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
execute m i = execute' m i

executeP :: Memory -> Instruction -> IO ()
executeP m i = do
    execute' m i
    printMemory m

execute' :: Memory -> Instruction -> IO ()
execute' m (SYS a)          = return () -- todo
execute' m CLS              = return () -- todo
execute' m RET              = return () -- todo
execute' m (JP a)           = return () -- todo
execute' m (CALL a)         = return () -- todo
execute' m (SEByte  vx w)   = return () -- todo
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
    (Mem8 x) <- load m (Register vx)
    result   <- loadf (x .|.) m (Register vy)
    store m (Register vx) result
execute' m (AND     vx vy)  = do
    (Mem8 x) <- load m (Register vx)
    result   <- loadf (x .&.) m (Register vy)
    store m (Register vx) result
execute' m (XOR     vx vy)  = do
    (Mem8 x) <- load m (Register vx)
    result   <- loadf (x `xor`) m (Register vy)
    store m (Register vx) result

loadf f m r = do
    (Mem8 x) <- load m r
    return (Mem8 $ f x)