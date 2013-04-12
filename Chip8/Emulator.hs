module Chip8.Emulator where

import Control.Monad
import Control.Monad.Random

import Chip8.CPU
import Chip8.Memory

import Data.Bits
import Data.Word

run :: [Instruction] -> IO ()
run is = do
    mem <- new
    mapM_ (execute mem) is

runP :: [Instruction] -> IO ()
runP is = do
    mem <- new
    mapM_ (executeP mem) is
    printRamR mem (0,24)

execute :: Memory -> Instruction -> IO ()
execute m i = do
    execute' m i
    case i of      -- Don't increment program counter for jumps.
        (JP _)     -> return ()
        (CALL _)   -> return ()
        (LONGJP _) -> return ()
        _          -> incrementProgramCounter m

executeP :: Memory -> Instruction -> IO ()
executeP m i = do
    execute m i
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
execute' m (DRW vx vy nib) = do
    return () -- todo
execute' m (SKP vx)    = do
    return () -- todo
execute' m (SKNP vx)   = do
    return () -- todo
execute' m (LDVxDT vx) = do
    return () -- todo
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

