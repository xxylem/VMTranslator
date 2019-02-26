module Model where

-- ===== --
-- Model --
-- This module holds all the data definitions that the parser will parse into.

data Direction =
    PUSH
  | POP

data Segment =
    ARGUMENT
  | LOCAL
  | STATIC
  | CONSTANT
  | THIS
  | THAT
  | POINTER
  | TEMP

type Index = Integer

data MemoryAccessCommand =
    Direction Segment Index

data ArithLogicCommand =
    ADD 
  | SUB
  | NEG
  | EQ
  | GT
  | LT
  | AND
  | OR
  | NOT
 