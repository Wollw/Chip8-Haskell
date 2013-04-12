{-# LANGUAGE TemplateHaskell #-}
module Chip8.Memory
    ( Register (..)
    , Address  (Ram, Register, Pc, Sp)
    , Memory
    , MemoryValue (..)
    , toRegister
    , fromRegister
    , new
    , load
    , store
    , toString
    , printMemory
    , incrementProgramCounter
    , popStack
    , pushStack
    , toMem8
    , toMem16
    ) where

import Chip8.Util

import Control.Lens
import Control.Monad.State

import Data.Word
import Data.Array.IO
import Data.IORef
import Data.List (intercalate)

data Register
    = V0 | V1 | V2 | V3
    | V4 | V5 | V6 | V7
    | V8 | V9 | VA | VB
    | VC | VD | VE | VF
    | I
    deriving (Bounded, Enum, Eq, Show)

toRegister  :: Int -> Register
toRegister = toEnum

fromRegister :: Register -> Int
fromRegister = fromEnum

data Address
    = Pc
    | Sp
    | Register Register
    | Ram Word16
    deriving (Eq)

instance Show Address where
    show Pc           = "Pc"
    show Sp           = "Sp"
    show (Register r) = show r
    show (Ram r)      = "[" ++ prettifyWord16 r ++ "]"

data Memory
    = Memory { pc        :: IORef Word16
             , sp        :: IORef Word8
             , registers :: IOUArray Int Word8 
             , iregister :: IORef Word16
             , ram       :: IOUArray Word16 Word8
             , stack     :: IOUArray Word8  Word16
             }

new :: IO Memory
new = do
    pc'        <- newIORef 0
    sp'        <- newIORef 0
    registers' <- newArray (0x0,   0xF  ) 0
    iregister' <- newIORef 0
    ram'       <- newArray (0x000, 0xFFF) 0
    stack'     <- newArray (0x00, 0x0f)   0
    return Memory { pc = pc'
                  , sp = sp'
                  , registers = registers'
                  , iregister = iregister'
                  , ram   = ram'
                  , stack = stack'
                  }

data MemoryValue
    = Mem8  Word8
    | Mem16 Word16
    deriving (Show)

toString :: Memory -> IO String
toString m = do
    pc'    <- fmap prettifyWord16       . readIORef . pc $ m
    sp'    <- fmap prettifyWord8        . readIORef . sp $ m
    regs'  <- fmap (map prettifyWord8)  . getElems  . registers $ m
    i'     <- fmap prettifyWord16       . readIORef . iregister $ m
    stack' <- fmap (map prettifyWord16) . getElems  . stack $ m
--  ram'   <- fmap (map prettifyWord8)  . getElems  . ram $ m
    return   $  "        PC: " ++ pc'
     ++ "\n" ++ " Registers: " ++ "| " ++ intercalate " | " (map show $ take 8 [V0 .. I]) ++ " |"
     ++ "\n" ++ "            " ++ "| " ++ intercalate " | " (take 8 regs') ++ " | "
     ++ "\n" ++ "            " ++ "| " ++ intercalate " | " (map show $ drop 8 [V0 .. I]) ++ "     |"
     ++ "\n" ++ "            " ++ "| " ++ intercalate " | " (drop 8 regs') ++ " | " ++ i' ++    "  |"
     ++ "\n" ++ "        SP: " ++ sp'
     ++ "\n" ++ "     Stack: " ++ "[" ++ intercalate ":" (take 8 stack') ++ ":"
     ++ "\n" ++ "            " ++ " " ++ intercalate ":" (drop 8 stack')  ++ "]"
--   ++ "\n" ++ "       RAM: " ++ "[" ++ intercalate ", " ram'  ++ "]"

printMemory :: Memory -> IO ()
printMemory m = toString m >>= putStrLn

load :: Memory -> Address -> IO MemoryValue
load m Pc           = (readIORef . pc $ m) >>= \x -> return $ Mem16 x
load m Sp           = (readIORef . sp $ m) >>= \x -> return $ Mem8  x
load m (Register I) = (readIORef . iregister $ m) >>= \x -> return $ Mem16  x
load m (Register r) = readArray (registers m) (fromEnum $ r) >>= \x -> return (Mem8 x)
load m (Ram r)      = readArray (ram m) r >>= \x -> return (Mem8 x)

store :: Memory -> Address -> MemoryValue -> IO ()
store m Pc (Mem16 x) = writeIORef (pc m) x
store m Sp (Mem8  x) = writeIORef (sp m) x
store m (Register I) (Mem16 x) = writeIORef (iregister m) x
store m (Register r) (Mem8  x) = writeArray (registers m) (fromEnum r) x
store m (Ram r)      (Mem8  x) = writeArray (ram m) r x

incrementProgramCounter :: Memory -> IO ()
incrementProgramCounter m = do
    (Mem16 pc) <- load m Pc
    store m Pc $ toMem16 (pc + 0x02)

popStack :: Memory -> IO Word16
popStack m = do
    (Mem8 sp)   <- load m Sp
    store m Sp $ toMem8 $ sp - 1
    readArray (stack m) $ sp - 1

pushStack :: Memory -> Word16 -> IO ()
pushStack m addr = do
    (Mem8 sp)   <- load m Sp
    store m Sp $ toMem8 (sp + 1)
    writeArray (stack m) sp addr

toMem8  x = Mem8  $ fromIntegral x
toMem16 x = Mem16 $ fromIntegral x
