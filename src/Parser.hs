{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Model

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8

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

parseMemoryAccessCommand :: Parser MemoryAccessCommand
parseMemoryAccessCommand = do
    direction <- parseDirection
    skipMany1 (char ' ')
    segment   <- parseSegment
    skipMany1 (char ' ')
    index     <- decimal
    return $  MemCMD direction segment index

parseArithLogicCMD :: Parser ArithLogicCommand
parseArithLogicCMD =
        (string "add" >> return ADD)
    <|> (string "sub" >> return SUB)
    <|> (string "neg" >> return NEG)
    <|> (string "eq"  >> return EQ_VM)
    <|> (string "gt"  >> return GT_VM)
    <|> (string "lt"  >> return LT_VM)
    <|> (string "and" >> return AND)
    <|> (string "or"  >> return OR)
    <|> (string "not" >> return NOT)