{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Model

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Conversion (toByteString')


-- ====== --
-- Parser --
-- This module parses a single .vm file into its constituent components and
-- provides access to said components.

parseDirection :: Parser Direction
parseDirection =
        (string "push" >> return PUSH)
    <|> (string "pop"  >> return POP)

parseSegment :: Parser Segment
parseSegment =
        (string "argument" >> return ARGUMENT)
    <|> (string "local"    >> return LOCAL)
    <|> (string "static"   >> return STATIC)
    <|> (string "constant" >> return CONSTANT)
    <|> (string "this"     >> return THIS)
    <|> (string "that"     >> return THAT)
    <|> (string "pointer"  >> return POINTER)
    <|> (string "temp"     >> return TEMP)

parseMemoryAccessCMD :: Parser MemoryAccessCommand
parseMemoryAccessCMD = do
    direction <- parseDirection
    skipMany1 (char ' ')
    segment   <- parseSegment
    skipMany1 (char ' ')
    index     <- decimal
    parseComment
    return $  MemCMD direction segment index

parseArithLogicCMD :: Parser ArithLogicCommand
parseArithLogicCMD =
    (       (string "add" >> return ADD)
        <|> (string "sub" >> return SUB)
        <|> (string "neg" >> return NEG)
        <|> (string "eq"  >> return EQ_VM)
        <|> (string "gt"  >> return GT_VM)
        <|> (string "lt"  >> return LT_VM)
        <|> (string "and" >> return AND)
        <|> (string "or"  >> return OR)
        <|> (string "not" >> return NOT)
    )
    <*  parseComment

parseLabelCommand :: Parser ProgramFlowCommand
parseLabelCommand = 
        string "label"
    >>  skipMany1 (char ' ')
    >>  LABEL <$> takeWhile1 (not . isSpace)

parseGotoCommand :: Parser ProgramFlowCommand
parseGotoCommand =
        string "goto"
    >>  skipMany1 (char ' ')
    >>  GOTO <$> takeWhile1 (not . isSpace)

parseIfGotoCommand :: Parser ProgramFlowCommand
parseIfGotoCommand =
        string "if-goto"
    >>  skipMany1 (char ' ')
    >>  IF_GOTO <$> takeWhile1 (not . isSpace)

parseProgramFlowCommand :: Parser ProgramFlowCommand
parseProgramFlowCommand =
    (       parseLabelCommand
        <|> parseGotoCommand
        <|> parseIfGotoCommand
    )
    <*  parseComment

parseFnCommand :: Parser FunctionCommand
parseFnCommand = do
    _       <- string "function"
    skipMany1 (char ' ')
    fnName  <- takeWhile1 (not . isSpace)
    skipMany1 (char ' ')
    nVars   <- decimal
    return $ FUN fnName nVars

parseCallCommand :: Parser FunctionCommand
parseCallCommand = do
    _       <- string "call"
    skipMany1 (char ' ')
    fnName  <- takeWhile1 (not . isSpace)
    skipMany1 (char ' ')
    nArgs   <- decimal
    return $ CALL fnName nArgs

parseReturnCommand :: Parser FunctionCommand
parseReturnCommand =
        string "return"
    >>  return RETURN

parseFunctionCommand :: Parser FunctionCommand
parseFunctionCommand =
    (       parseFnCommand
        <|> parseCallCommand
        <|> parseReturnCommand
    )
    <*  parseComment

parseVMLine :: Parser VMLine
parseVMLine = 
        (AL_VM <$> parseArithLogicCMD)
    <|> (M_VM  <$> parseMemoryAccessCMD)
    <|> (P_VM  <$> parseProgramFlowCommand)
    <|> (F_VM  <$> parseFunctionCommand)

type ErrorMsg = String

newtype ParseError = InvalidLine ErrorMsg
                    deriving (Eq, Show)

parseComment :: Parser ()
parseComment =
        skipSpace 
    >>  (   (string "//" >> return ())
        <|> endOfInput
        )

removeCommentsAndEmptyLines :: [BS.ByteString] 
                            -> [BS.ByteString]
removeCommentsAndEmptyLines = filter (not . runParseIsEmptyLineOrComment)
            where runParseIsEmptyLineOrComment l =
                    case parseOnly parseComment l of
                        (Right _) -> True
                        (Left _)  -> False

parseVMFile :: UnparsedVMFile -> Either ParseError VMFile
parseVMFile (ls, fp) = case go $ removeCommentsAndEmptyLines ls of
                            Right file -> Right (file, toByteString' fp)
                            Left  err  -> Left err
        where   go []     = Right []
                go (l:ls) =
                    case parseOnly parseVMLine l of
                    (Right cmd) -> (:) <$> Right cmd <*> go ls
                    (Left err)  -> Left $ InvalidLine err

parseVMFiles :: UnparsedVMProgram -> Either ParseError VMProgram
parseVMFiles [] = return []
parseVMFiles (f:fs) = (:) <$> (parseVMFile f) <*> parseVMFiles fs