module Model where

-- ===== --
-- Model --
-- This module holds all the data definitions that the parser will parse into.

data Direction =
    PUSH
  | POP
  deriving (Eq, Show)

data Segment =
    ARGUMENT
  | LOCAL
  | STATIC
  | CONSTANT
  | THIS
  | THAT
  | POINTER
  | TEMP
  deriving (Eq, Show)

type Index = Integer

data MemoryAccessCommand =
    MemCMD Direction Segment Index
    deriving (Eq, Show)

data ArithLogicCommand =
    ADD 
  | SUB
  | NEG
  | EQ_VM
  | GT_VM
  | LT_VM
  | AND
  | OR
  | NOT
  deriving (Eq, Show)
 