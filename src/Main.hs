{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Parser
import Model

import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Conversion (toByteString')
import ReadArgs (readArgs)
import System.Directory (listDirectory)
import System.FilePath (dropExtension, hasExtension, isExtensionOf, (</>), takeBaseName)
import System.IO

-- ============= --
-- VM Translator --
-- The VM translator will take as input one .vm file or multiple .vm files in a directory and output
-- a single .asm file.

initState :: Handle -> WritingStates
initState = LS 0 0 0 0 Outside

writeInitCode :: Handle -> IO WritingStates
writeInitCode h =
    let (sts, sysInitCall) = toASM (CALL "Sys.init" 0) (initState h) "" in
    BS.hPutStrLn h
        (       "    //init code\n"
            <>  "    @256\n"
            <>  "    D=A\n"
            <>  "    @SP\n"
            <>  "    M=D\n"
            <>  sysInitCall
        )
    *> return sts

translateDirectory :: FilePath -> IO ()
translateDirectory dirPath = do
    dirContents <- listDirectory dirPath
    let vmFileNames = map (dirPath </>) $ filter (".vm" `isExtensionOf`) dirContents
    vmFiles <- readVMFiles vmFileNames
    case parseVMFiles vmFiles of
        Right program -> writeVMProgram program dirPath
        Left  err     -> print err

translateSingleFile :: FilePath -> IO ()
translateSingleFile filePath = do
    vmFile <- readVMFile filePath
    case parseVMFile vmFile of
        Right program -> writeSingleVMFile program ((dropExtension filePath) ++ ".asm")
        Left err -> putStrLn ("Parse error: "
                                <> show err)

readVMFile :: FilePath -> IO UnparsedVMFile
readVMFile fp = do
    file <- BS.readFile fp
    return (BS.lines file, takeBaseName fp)

readVMFiles :: [FilePath] -> IO UnparsedVMProgram
readVMFiles [] = return []
readVMFiles (f:fs) = (:) <$> readVMFile f <*> readVMFiles fs

writeVMProgram :: VMProgram -> FilePath -> IO ()
writeVMProgram vmProgram dirPath =
    withFile (dirPath </> dirPath ++ ".asm") WriteMode (\h -> do
        sts <- writeInitCode h
        writeVMFiles sts vmProgram)

writeVMFiles :: WritingStates -> VMProgram -> IO ()
writeVMFiles _ [] = return ()
writeVMFiles sts (f:fs) = do
    sts' <- writeVMFile sts f
    writeVMFiles sts' fs

writeVMFile :: WritingStates -> VMFile -> IO WritingStates
writeVMFile sts                        ([],_)     = return sts
writeVMFile sts@(LS e g l r funName h) (line:ls, fileName) =
    let (sts', code) = toASM line sts fileName in
            BS.hPutStr h code
        >>  BS.hPutStr h "\n"
        >>  writeVMFile sts' (ls, fileName)

writeSingleVMFile :: VMFile -> FilePath -> IO ()
writeSingleVMFile vmFile filePath =
    withFile filePath WriteMode (\h -> 
        writeVMFile (initState h) vmFile >> return ())

main :: IO ()
main = do
    (path :: FilePath) <- readArgs
    if hasExtension path
        then if ".vm" `isExtensionOf` path 
                    then translateSingleFile path 
                    else putStrLn $ "Usage: VMTranslator.exe \"file.vm\"\n"
                                <>  "\tVMTranslator.exe \"directory\"\n"
        else translateDirectory path