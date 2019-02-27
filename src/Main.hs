module Main where

import Parser
import CodeWriter
import Model

import qualified Data.ByteString.Char8 as BS
import System.FilePath (dropExtension)

-- ============= --
-- VM Translator --
-- The VM translator will take as input one .vm file or multiple .vm files in a directory and output
-- a single .asm file.

-- The language consists of four types of
--     commands:
--     Arithmetic commands perform arithmetic and logical operations on the stack.
--     Memory access commands transfer data between the stack and virtual memory
--     segments.
--     Program flow commands facilitate conditional and unconditional branching
--     operations.
--     Function calling commands call functions and return from them.

-- First: Specify arithmetic and memory access commands.
--          Build VM translator that implements them
{- 
Within a .vm file, each VM command appears in a separate line, and in one of the
following formats: command (e.g., add), command arg (e.g., goto loop), or command
arg1 arg2 (e.g., push local 3). The arguments are separated from each other and
from the command part by an arbitrary number of spaces. ‘‘//’’ comments can appear
at the end of any line and are ignored. Blank lines are permitted and ignored. -}

--  command
--  command arg
--  command arg1 arg2

--   parts separated by arbitary number of spaces

--    // comments
--   blank lines ignored

{- 
Command
Return value (after
popping the operand/s) Comment
add x y ---> x + y Integer addition (2’s complement)
sub x y ---> x - y Integer subtraction (2’s complement)
neg y   ---> -y Arithmetic negation (2’s complement)
eq  x y ---> true if x = y, else false Equality
gt  x y ---> true if x > y, else false Greater than
lt  x y ---> true if x < y, else false Less than
and x y ---> x And y Bit-wise
or  x y ---> x Or y Bit-wise
not   y ---> Not y Bit-wise -}

-- TODO create data structure for these arithmetic/logic commands
--    vm stack commands will be parsed into this data format
--      then they can later be output to approp. assembly code

-- TODO data structure needs to remember originating file, if there is more than one file

-- TODO: Stage 1:
-- Stack arithmetic commands. Implement the nine stack arithmetic and logical commands, as
-- well as push constant x.


writeProgramToFile :: FilePath -> Program -> IO ()
writeProgramToFile fp program = undefined --TODO

main :: IO ()
main = putStrLn "Hello, Haskell!"

handleSingleFile :: FilePath -> IO ()
handleSingleFile fp = do
    file <- BS.readFile fp
    case parseVMLines $ BS.lines file of
        Right program -> writeProgramToFile (changeExt fp) $! program
                where changeExt fp = dropExtension fp ++ ".asm"
        Left err -> putStrLn ("Parse error: "
                                <> show err)


{- 
Memory Access Commands All the memory segments are accessed by the same two
commands:
push segment index Push the value of segment[index] onto the stack.
pop segment index Pop the top stack value and store it in segment[index]. -}