{-# LANGUAGE OverloadedStrings #-}

module Model where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Conversion (toByteString')

-- ===== --
-- Model --
-- This module holds all the data definitions that the parser will parse into.

class ToASMCode a where
  toASM :: a -> LabelStates -> (LabelStates, BS.ByteString)

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


instance ToASMCode MemoryAccessCommand where
  toASM (MemCMD PUSH CONSTANT x) sts =
    (sts,
      let xBs = toByteString' x <> "\n" in
          "    //push constant " <> xBs
      <>  "    @" <> xBs
      <>  "    D=A\n"
      <>  "    @SP\n"
      <>  "    A=M\n"
      <>  "    M=D\n"
      <>  "    @SP\n"
      <>  "    M=M+1\n")
      -- TODO use builders or just one long string - slow implem.


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

type EqLabel = Integer
type GtLabel = Integer
type LtLabel = Integer

data LabelStates = LS EqLabel GtLabel LtLabel
                  deriving (Eq, Show)

instance ToASMCode ArithLogicCommand where
  toASM ADD sts =
    (sts,
        "    //add\n"
    <>  "    @SP\n"
    <>  "    A=M\n"
    <>  "    D=M\n"
    <>  "    A=A-1\n"
    <>  "    M=M+D\n"
    <>  "    @SP\n"
    <>  "    M=M-1\n")

  toASM SUB sts =
    (sts,
        "    //sub\n"
    <>  "    @SP\n"
    <>  "    A=M\n"
    <>  "    D=M\n"
    <>  "    A=A-1\n"
    <>  "    M=M-D\n"
    <>  "    @SP\n"
    <>  "    M=M-1\n")

  toASM NEG sts =
    (sts, 
        "    //neg\n"
    <>  "    @SP\n"
    <>  "    A=M\n"
    <>  "    M=-M\n")

  toASM EQ_VM (LS e g l) =
    (LS (e+1) g l,
      let trueJmp = "TRUE_EQ_" <> toByteString' e
          endJmp  = "END_EQ_"  <> toByteString' e in
            "    //eq\n"
        <>  "    @SP\n"
        <>  "    A=M\n"
        <>  "    D=M\n"
        <>  "    @SP\n"
        <>  "    M=M-1\n"
        <>  "    A=M\n"
        <>  "    D=D-M\n"
        <>  "    @" <> trueJmp <> "\n"
        <>  "    D;JEQ\n"
        <>  "    @SP\n"
        <>  "    A=M\n"
        <>  "    M=0\n"
        <>  "    @" <> endJmp <> "\n"
        <>  "    0;JMP\n"
        <>  "(" <> trueJmp <> ")\n"
        <>  "    @SP\n"
        <>  "    A=M\n"
        <>  "    M=-1\n"
        <>  "(" <> endJmp <> ")\n")

  toASM GT_VM (LS e g l) =
    (LS e (g+1) l,
      let trueJmp = "TRUE_GT_" <> toByteString' g
          endJmp  = "END_GT_"  <> toByteString' g in
            "    //gt\n"
        <>  "    @SP\n"
        <>  "    A=M\n"
        <>  "    D=M\n"
        <>  "    @SP\n"
        <>  "    M=M-1\n"
        <>  "    A=M\n"
        <>  "    D=D-M\n"
        <>  "    @" <> trueJmp <> "\n"
        <>  "    D;JLT\n"
        <>  "    @SP\n"
        <>  "    A=M\n"
        <>  "    M=0\n"
        <>  "    @" <> endJmp <> "\n"
        <>  "    0;JMP\n"
        <>  "(" <> trueJmp <> ")\n"
        <>  "    @SP\n"
        <>  "    A=M\n"
        <>  "    M=-1\n"
        <>  "(" <> endJmp <> ")\n")
  
  toASM LT_VM (LS e g l) =
    (LS e g (l+1),
      let trueJmp = "TRUE_LT_" <> toByteString' l
          endJmp  = "END_LT_"  <> toByteString' l in
            "    //lt\n"
        <>  "    @SP\n"
        <>  "    A=M\n"
        <>  "    D=M\n"
        <>  "    @SP\n"
        <>  "    M=M-1\n"
        <>  "    A=M\n"
        <>  "    D=D-M\n"
        <>  "    @" <> trueJmp <> "\n"
        <>  "    D;JGT\n"
        <>  "    @SP\n"
        <>  "    A=M\n"
        <>  "    M=0\n"
        <>  "    @" <> endJmp <> "\n"
        <>  "    0;JMP\n"
        <>  "(" <> trueJmp <> ")\n"
        <>  "    @SP\n"
        <>  "    A=M\n"
        <>  "    M=-1\n"
        <>  "(" <> endJmp <> ")\n")

  toASM AND sts =
    (sts,
        "    //and\n"
    <>  "    @SP\n"
    <>  "    A=M\n"
    <>  "    D=M\n"
    <>  "    A=A-1\n"
    <>  "    M=D&M\n"
    <>  "    @SP\n"
    <>  "    M=M-1\n")

  toASM OR sts =
    (sts,
        "    //or\n"
    <>  "    @SP\n"
    <>  "    A=M\n"
    <>  "    D=M\n"
    <>  "    A=A-1\n"
    <>  "    M=D|M\n"
    <>  "    @SP\n"
    <>  "    M=M-1\n")

  toASM NOT sts =
    (sts,
        "    //not\n"
    <>  "    @SP\n"
    <>  "    A=M\n"
    <>  "    M=!M\n")

 
data VMLine =
    AL_VM ArithLogicCommand
  | M_VM  MemoryAccessCommand
  deriving (Eq, Show)

instance ToASMCode VMLine where
  toASM (AL_VM c) = toASM c
  toASM (M_VM  c) = toASM c

type Program = [VMLine]
