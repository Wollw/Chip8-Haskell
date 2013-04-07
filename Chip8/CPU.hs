module Chip8.CPU where

import Chip8.Memory
import Chip8.Util

import Data.Bits
import Data.Word

data Instruction 
    = SYS       Address
    | CLS
    | RET
    | JP        Address
    | CALL      Address
    | SEByte    Address Word8
    | SNEByte   Address Word8
    | SEAddr    Address Address
    | LDByte    Address Word8
    | ADDByte   Address Word8
    | LDReg     Address Address
    | OR        Address Address
    | AND       Address Address
    | XOR       Address Address
    | ADDReg    Address Address
    | SUB       Address Address
    | SHR       Address
    | SUBN      Address Address
    | SHL       Address
    | SNEAddr   Address Address
    | LDI       Address
    | LONGJP    Address
    | RND       Address Word8
    | DRW       Address Address Word8
    | SKP       Address
    | SKNP      Address
    | LDVxDT    Address
    | LDKey     Address
    | LDDTVx    Address
    | LDSTVx    Address
    | ADDI      Address
    | LDF       Address
    | LDB       Address
    | LDRegsToI   Address
    | LDRegsFromI Address
    deriving (Show)

decodeInstruction :: Word16 -> Instruction
decodeInstruction op = case nibble 3 op of
    0x0 -> case op of
        0x00E0 -> CLS
        0x00EE -> RET
        _      -> SYS (Ram op)
    0x1 -> JP   (Ram $ 0x0fff .&. op)
    0x2 -> CALL (Ram $ 0x0fff .&. op)
    0x3 -> SEByte  vx (lowByte op)
    0x4 -> SNEByte vx (lowByte op)
    0x5 -> SEAddr  vx vy
    0x6 -> LDByte  vx (lowByte op)
    0x7 -> ADDByte vx (lowByte op)
    0x8 -> case 0xf00f .&. op of
        0x8000 -> LDReg   vx vy
        0x8001 -> OR      vx vy
        0x8002 -> AND     vx vy
        0x8003 -> XOR     vx vy
        0x8004 -> ADDReg  vx vy
        0x8005 -> SUB     vx vy
        0x8006 -> SHR     vx
        0x8007 -> SUBN    vx vy
        0x8008 -> SHL     vx
    0x9 -> SNEAddr vx vy
    0xA -> LDI     (Ram $ 0x0fff .&. op)
    0xB -> LONGJP  (Ram $ 0x0fff .&. op)
    0xC -> RND     vx (lowByte op)
    0xD -> DRW     vx vy (fromIntegral . nibble 0 $ op)
    0xE -> case 0xf0ff .&. op of
        0xE09E -> SKP  vx
        0xE0A1 -> SKNP vx
    0xF -> case 0xf0ff .&. op of
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
    vx = nibToReg 2 op
    vy = nibToReg 1 op
    nibToReg n w = Register (toRegister . fromIntegral . nibble n $ w)
