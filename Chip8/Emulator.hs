module Chip8.Emulator where

import Chip8.CPU
import Chip8.Memory

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
