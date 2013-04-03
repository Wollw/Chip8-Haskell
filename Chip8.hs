{-# LANGUAGE TemplateHaskell #-}

import Control.Lens

import Data.Word
import Data.Array.IO
import Data.Bits
import qualified Data.ByteString as B

import Numeric

import System.Random
import System.IO

type EmuOp = Word16

data EmuState = EmuState
    { vRegisters :: IOUArray Word8  Word8
    , _iRegister :: Word16
    , memory     :: IOUArray Word16 Word8
    , stack      :: IOUArray Word8  Word16
    , _pc        :: Word16
    , _sp        :: Word8
    , _randGen   :: StdGen
    }

makeLenses ''EmuState

newEmuState :: [Word8] -> IO EmuState
newEmuState ops = do
    regs <- newArray (0, 15) 0
    mem  <- newListArray (0,0xFFF) $ memStart ++ ops ++ memEnd
    stk  <- newArray (0, 15) 0
    gen  <- getStdGen
    return EmuState
        { vRegisters = regs
        , _iRegister = 0
        , memory     = mem
        , stack      = stk
        , _pc = 0x200
        , _sp = 0x00
        , _randGen = gen
        }
    where
        memStart = replicate 0x200 0
        memEnd   = replicate (0xFFF - 0x200 - (length ops)) 0

runEmuState :: EmuState -> IO EmuState
runEmuState emu = do
    b1 <- readArray (memory emu) $ view pc emu
    b2 <- readArray (memory emu) $ view pc emu + 1
    let op =   (fromIntegral b1 :: Word16) `shift` 8
             + (fromIntegral b2 :: Word16)
    case op of 
        0x0000 -> return emu
        _      -> runEmuOp emu op >>= runEmuState

runEmuOp :: EmuState -> EmuOp -> IO EmuState
runEmuOp emu op = do
    print $ "0x" ++ (showHex op "")
    case op of
        0x00E0 -> op_CLS -- Need to implement
        0x00EE -> op_RET
        op     -> case op .&. 0xF000 of
            0x1000 -> op_JP_addr
            0x2000 -> op_CALL_addr
            0x3000 -> op_SE_Vx_byte
            0x4000 -> op_SNE_Vx_byte
            0x5000 -> op_SE_Vx_Vy
            0x6000 -> op_LD_Vx_byte
            0x7000 -> op_ADD_Vx_byte
            0x8000 -> case op .&. 0xF00F of
                0x8000 -> op_LD_Vx_Vy
                0x8001 -> op_OR_Vx_Vy
                0x8002 -> op_AND_Vx_Vy
                0x8003 -> op_XOR_Vx_Vy
                0x8004 -> op_ADD_Vx_Vy
                0x8005 -> op_SUB_Vx_Vy
                0x8006 -> op_SHR_Vx
                0x8007 -> op_SUBN_Vx_Vy
                0x800E -> op_SHL_Vx
            0x9000 -> op_SNE_Vx_Vy
            0xA000 -> op_LD_I_addr
            0xB000 -> op_JP_V0_addr
            0xC000 -> op_RND_Vx_byte
            0xD000 -> op_DRW_Vx_Vy_nibble
            0xE000 -> case op .&. 0xF0FF of
                0xE09E -> op_SKP_Vx isPressed
                0xE0A1 -> op_SKNP_Vx isPressed
            0xF000 -> case op .&. 0xF0FF of
                0xF007 -> op_LD_Vx_DT
                0xF00A -> op_LD_Vx_K
                0xF015 -> op_LD_DT_Vx
                0xF018 -> op_LD_ST_Vx
                0xF01E -> op_ADD_I_Vx
                0xF029 -> op_LD_F_Vx
                0xF033 -> op_LD_B_Vx
                0xF055 -> op_LD_I_Vx
                0xF065 -> op_LD_Vx_I
            _      -> do
                putStrLn ("ERROR: Invalid Opcode 0x" ++ (showHex op ""))
                return . pcInc $ emu
  where
    -- Clear the display --
    op_CLS = return emu
    -- Return from a subroutine --
    op_RET = do
        addr <- readArray (stack emu) (view sp emu)
        return . spDec . set pc addr $ emu
    -- Jump to location --
    op_JP_addr    = do
        return . set pc (op .&. 0x0FFF) $ emu
    -- Call subroutine at location --
    op_CALL_addr  = do
        writeArray (stack emu) (view sp emu) (view pc emu)
        return . set pc (op .&. 0x0FFF) . spInc $ emu
    -- Skip if Vc == value --
    op_SE_Vx_byte = do
        vX <- readArray (vRegisters emu) (nybble 2 op)
        skipIf (==) vX (fromIntegral $ op .&. 0xff)
    -- Skip if Vx /= value --
    op_SNE_Vx_byte = do
        vX <- readArray (vRegisters emu) (nybble 2 op)
        skipIf (/=) vX (fromIntegral $ op .&. 0xff)
    -- Skip if Vx == Vy --
    op_SE_Vx_Vy = do
        (vX, vY) <- getVxVy
        skipIf (==) vX vY
    -- Set register value --
    op_LD_Vx_byte = do
        writeArray (vRegisters emu) (nybble 2 op) (byte 0 op)
        return . pcInc $ emu
    -- Set Vx to Vx + value --
    op_ADD_Vx_byte = do
        vX <- readArray (vRegisters emu) (nybble 2 op)
        writeArray (vRegisters emu) (nybble 2 op) $ byte 0 op + vX
        return . pcInc $ emu
    -- Set Vx to Vy --
    op_LD_Vx_Vy = do
        vY <- readArray (vRegisters emu) (nybble 1 op)
        writeArray (vRegisters emu) (nybble 2 op) vY
        return . pcInc $ emu
    -- operations on Vx and Vy that store to Vx --
    op_OR_Vx_Vy  = opVxVy (.|.)
    op_AND_Vx_Vy = opVxVy (.&.)
    op_XOR_Vx_Vy = opVxVy xor
    op_ADD_Vx_Vy = do
        (vX, vY) <- getVxVy
        case fromIntegral vX + fromIntegral vY > 255 of
            True  -> setVx (nybble 2 op) (vX + vY) emu >>= setVx 0xf 1 >>= (\e -> return $ pcInc e)
            False -> setVx (nybble 2 op) (vX + vY) emu >>= setVx 0xf 0 >>= (\e -> return $ pcInc e)
    op_SUB_Vx_Vy = do
        (vX, vY) <- getVxVy
        case vX > vY of
            True  -> setVx (nybble 2 op) (vX - vY) emu >>= setVx 0xf 1 >>= (\e -> return $ pcInc e)
            False -> setVx (nybble 2 op) (vX - vY) emu >>= setVx 0xf 0 >>= (\e -> return $ pcInc e)
    op_SHR_Vx = do
        (vX, _) <- getVxVy
        setVx (nybble 2 op) (vX `shiftR` 1) emu >>= setVx 0xf (vX .&. 1) >>= (\e -> return $ pcInc e)
    op_SUBN_Vx_Vy = do
        (vX, vY) <- getVxVy
        case vY > vX of
            True  -> setVx (nybble 2 op) (vY - vX) emu >>= setVx 0xf 1 >>= (\e -> return $ pcInc e)
            False -> setVx (nybble 2 op) (vY - vX) emu >>= setVx 0xf 0 >>= (\e -> return $ pcInc e)
    op_SHL_Vx = do
        (vX, _) <- getVxVy
        case vX .&. 128 /= 0 of
            True  -> setVx (nybble 2 op) (vX `shiftL` 1) emu >>= setVx 0xf 1 >>= (\e -> return $ pcInc e)
            False -> setVx (nybble 2 op) (vX `shiftL` 1) emu >>= setVx 0xf 0 >>= (\e -> return $ pcInc e)
    op_SNE_Vx_Vy = do
        (vX, vY) <- getVxVy
        skipIf (/=) vX vY
    op_LD_I_addr = do
        return . pcInc . set iRegister (op .&. 0x0fff) $ emu
    op_JP_V0_addr = do
        v0 <- getVx 0 emu
        return . set pc ((op .&. 0x0FFF) + (fromIntegral v0)) $ emu
    op_RND_Vx_byte = do
        let gen = view randGen emu
        let val = ((fst . next $ gen) `mod` 255) .&. (fromIntegral $ byte 0 op)
        let newGen = snd . next $ gen
        setVx (nybble 2 op) (fromIntegral val) $ pcInc . set randGen newGen $ emu

    op_DRW_Vx_Vy_nibble = return emu -- Need to implement
    op_SKP_Vx isPressed  = do
        pressed <- isPressed
        case pressed of
            True  -> print "Pressed."
            False -> print "Not pressed."
        return emu
    op_SKNP_Vx isPressed = do -- Need to implement
        return emu
    op_LD_Vx_DT = return emu
    op_LD_Vx_K  = return emu
    op_LD_DT_Vx = return emu
    op_LD_ST_Vx = return emu
    op_ADD_I_Vx = return emu
    op_LD_F_Vx  = return emu
    op_LD_B_Vx  = return emu
    op_LD_I_Vx  = return emu
    op_LD_Vx_I  = return emu
    
    pcInc e = set pc (view pc e + 2) e
    spDec e = set sp (view sp e - 1) e
    spInc e = set sp (view sp e + 1) e
    skipIf f x y = do
        case x `f` y of
            True  -> return . pcInc . pcInc $ emu
            False -> return . pcInc $ emu
    opVxVy f = do
        (vX, vY) <- getVxVy
        writeArray (vRegisters emu) (nybble 2 op) $ vX `f` vY
        return . pcInc $ emu
    getVxVy = do
        vX <- readArray (vRegisters emu) (nybble 2 op)
        vY <- readArray (vRegisters emu) (nybble 1 op)
        return (fromIntegral vX, fromIntegral vY)
    setVx vX val emu = do
        writeArray (vRegisters emu) vX val
        return emu
    getVx vX emu = readArray (vRegisters emu) vX
    isPressed = hReady stdin

nybble :: Int -> EmuOp -> Word8
nybble n op = fromIntegral $ (op .&. (0xf `shift` (n * 4))) `shift` (n * (-4))

byte :: Int -> EmuOp -> Word8
byte n op = fromIntegral (op .&. (0xff `shift` (n * 8))) `shift` (n * (-8))

printEmuState emu = do
    elems <- getElems . vRegisters $ emu
    putStrLn $ "V Registers: " ++ (show elems)
    putStrLn $ " I Register: " ++ (show . _iRegister $ emu)

main = do
    file <- B.readFile "ROM"
    emu <- newEmuState . B.unpack $ file
    runEmuState emu
    registers <- getElems . vRegisters $ emu
    print registers
