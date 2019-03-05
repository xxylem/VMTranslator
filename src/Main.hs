{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Parser
import Model

import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Conversion (toByteString')
import ReadArgs (readArgs)
import System.FilePath (dropExtension)
import System.IO

-- ============= --
-- VM Translator --
-- The VM translator will take as input one .vm file or multiple .vm files in a directory and output
-- a single .asm file.

initState :: LabelStates
initState = LS 0 0 0

writeProgramToFile :: FilePath -> Program -> IO ()
writeProgramToFile fp program =
    withFile fp WriteMode (\h -> writeProgram h program initState)
                where fileName                  = toByteString' $ dropExtension fp <> "."
                      writeProgram _ [] _       = return ()
                      writeProgram h (l:ls) sts = 
                        let (sts', code) = toASM l sts fileName in
                            BS.hPutStr h code
                        >>  BS.hPutStr h "\n"
                        >>  writeProgram h ls sts'

main :: IO ()
main = do
    (path :: FilePath) <- readArgs
    handleSingleFile path

handleSingleFile :: FilePath -> IO ()
handleSingleFile fp = do
    file <- BS.readFile fp
    case parseVMLines $ BS.lines file of
        Right program -> writeProgramToFile (changeExt fp) $! program
                where changeExt fp = dropExtension fp ++ ".asm"
        Left err -> putStrLn ("Parse error: "
                                <> show err)

-- todo: init code: start by generating assembly code that sets SP=256
--      then call Sys.init // Start executing (the translated code of) Sys.init

--todo: call f n
--todo: function f k
--todo: return

--todo: goto
--todo: if-goto
--todo: label