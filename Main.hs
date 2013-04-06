import Chip8.Memory

main = do
    m <- new
    store m (Ram 0x200) (Mem8 0xff)
    load m (Ram 0x200) >>= \(Mem8 x) -> print x
