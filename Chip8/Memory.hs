{-# LANGUAGE TemplateHaskell #-}
module Chip8.Memory
    ( Register (..)
    , Address  (Ram, Register)
    , Memory
    , MemoryValue (..)
    , toRegister
    , fromRegister
    , new
    , load
    , store
    , toString
    , printMemory
    ) where

import Chip8.Util

import Control.Lens
import Control.Monad.State

import Data.Word
import Data.Array.IO
import Data.IORef

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
             }

new :: IO Memory
new = do
    pc'        <- newIORef 0
    sp'        <- newIORef 0
    registers' <- newArray (0x0,   0xF  ) 0
    iregister' <- newIORef 0
    ram'       <- newArray (0x000, 0xFFF) 0
    return Memory { pc = pc'
                  , sp = sp'
                  , registers = registers'
                  , iregister = iregister'
                  , ram = ram'
                  }

data MemoryValue
    = Mem8  Word8
    | Mem16 Word16
    deriving (Show)

toString :: Memory -> IO String
toString m = do
    pc'   <- fmap show . readIORef . pc $ m
    sp'   <- fmap show . readIORef . sp $ m
    regs' <- fmap show . getElems  . registers $ m
    ir'   <- fmap show . readIORef . iregister $ m
    ram'  <- fmap show . getElems  . ram $ m
    return   $  "        PC: " ++ pc'
     ++ "\n" ++ "        SP: " ++ sp'
     ++ "\n" ++ " Registers: " ++ regs'
     ++ "\n" ++ "I Register: " ++ ir'
--   ++ "\n" ++ "       RAM: " ++ ram'

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
