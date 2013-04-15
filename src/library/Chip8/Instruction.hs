module Chip8.Instruction where

import Chip8.Memory
import Chip8.Util

import Data.Bits
import Data.Word

data Instruction 
    = SYS         Address
    | CLS
    | RET
    | JP          Address
    | CALL        Address
    | SEByte      Register Word8
    | SNEByte     Register Word8
    | SEAddr      Register Register
    | LDByte      Register Word8
    | ADDByte     Register Word8
    | LDReg       Register Register
    | OR          Register Register
    | AND         Register Register
    | XOR         Register Register
    | ADDReg      Register Register
    | SUB         Register Register
    | SHR         Register
    | SUBN        Register Register
    | SHL         Register
    | SNEAddr     Register Register
    | LDI         Address
    | LONGJP      Address
    | RND         Register Word8
    | DRW         Register Register Word8
    | SKP         Register
    | SKNP        Register
    | LDVxDT      Register
    | LDKey       Register
    | LDDTVx      Register
    | LDSTVx      Register
    | ADDI        Register
    | LDF         Register
    | LDB         Register
    | LDRegsToI   Register
    | LDRegsFromI Register
    deriving (Show)

decodeInstruction :: Word16 -> Instruction
decodeInstruction op = case nibble 3 op of
    0x0 -> case op of
        0x00E0 -> CLS
        0x00EE -> RET
        _      -> SYS (Ram op)
    0x1 -> JP   (Ram oxxx)
    0x2 -> CALL (Ram oxxx)
    0x3 -> SEByte  vx (lowByte op)
    0x4 -> SNEByte vx (lowByte op)
    0x5 -> SEAddr  vx vy
    0x6 -> LDByte  vx (lowByte op)
    0x7 -> ADDByte vx (lowByte op)
    0x8 -> case xoox of
        0x8000 -> LDReg   vx vy
        0x8001 -> OR      vx vy
        0x8002 -> AND     vx vy
        0x8003 -> XOR     vx vy
        0x8004 -> ADDReg  vx vy
        0x8005 -> SUB     vx vy
        0x8006 -> SHR     vx
        0x8007 -> SUBN    vx vy
        0x800E -> SHL     vx
    0x9 -> SNEAddr vx vy
    0xA -> LDI     (Ram oxxx)
    0xB -> LONGJP  (Ram oxxx)
    0xC -> RND     vx (lowByte op)
    0xD -> DRW     vx vy (nibble 0 op)
    0xE -> case xoxx .&. op of
        0xE09E -> SKP  vx
        0xE0A1 -> SKNP vx
    0xF -> case xoxx of
        0xF007 -> LDVxDT      vx
        0xF00A -> LDKey       vx
        0xF015 -> LDDTVx      vx
        0xF018 -> LDSTVx      vx
        0xF01E -> ADDI        vx
        0xF029 -> LDF         vx
        0xF033 -> LDB         vx
        0xF055 -> LDRegsToI   vx
        0xF065 -> LDRegsFromI vx
  where
    vx = toRegister . nibble 2 $ op
    vy = toRegister . nibble 1 $ op
    xoox = 0xF00F .&. op 
    xoxx = 0xF0FF .&. op 
    oxxx = 0x0FFF .&. op 
