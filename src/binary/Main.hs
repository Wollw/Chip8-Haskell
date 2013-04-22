import Chip8
import System.Environment
main = getArgs >>= runFile . head

