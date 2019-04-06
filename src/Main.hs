{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main (main) where

import qualified Data.Source.Model as SRC
import qualified Parser.VM as P
import qualified Data.Output.Model as OUT
import qualified Data.VM.ConversionTo.ASMCode as VM2ASM
import qualified Data.Hack.ASM.ConversionTo.ByteString as ASM2BS

import qualified Data.ByteString.Char8 as BS
import ReadArgs (readArgs)
import System.Directory (listDirectory)
import System.FilePath (hasExtension, isExtensionOf, (</>))

-- ============= --
-- VM Translator --
-- The VM translator will take as input one .vm file or multiple .vm files in a directory and output
-- a single .asm file.

main :: IO ()
main = do
    (path :: FilePath) <- readArgs
    if hasExtension path
        then if ".vm" `isExtensionOf` path 
                    then translateSingleFile path 
                    else putStrLn $ "Usage: VMTranslator.exe \"file.vm\"\n"
                                <>  "\tVMTranslator.exe \"directory\"\n"
        else translateDirectory path

readBSFiles :: [FilePath] -> IO [(FilePath, BS.ByteString)]
readBSFiles [] = return []
readBSFiles (f:fs) = do
    f' <- BS.readFile f
    fs' <- readBSFiles fs
    return ((f, f') : fs')

translateDirectory :: FilePath -> IO ()
translateDirectory dirPath = do
    dirContents <- listDirectory dirPath
    let vmFileNames = map (dirPath </>) $ filter (".vm" `isExtensionOf`) dirContents
    bsFiles <- readBSFiles vmFileNames
    let unparsedFiles = map (\(fp, bsFile) -> SRC.toUnparsedFile fp 0 bsFile) bsFiles
    case P.parseVMFiles unparsedFiles of
        Right vmFiles -> (OUT.writeOutputFile . ASM2BS.convert) $ VM2ASM.convertDirectory 
                                dirPath vmFiles
        Left  err     -> print err

translateSingleFile :: FilePath -> IO ()
translateSingleFile filePath = do
    bsFile <- BS.readFile filePath
    let unparsedFile = SRC.toUnparsedFile filePath 0 bsFile
    case P.parseVMFile unparsedFile of
        Right vmFile -> (OUT.writeOutputFile . ASM2BS.convert . VM2ASM.convertSingleFile) vmFile
        Left err -> putStrLn ("Parse error: "
                                <> show err)
