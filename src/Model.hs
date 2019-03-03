{-# LANGUAGE OverloadedStrings #-}

module Model where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Conversion (toByteString')

type FileName = BS.ByteString
type ASMCode  = BS.ByteString

-- ===== --
-- Model --
-- This module holds all the data definitions that the parser will parse into.

class ToASMCode a where
  toASM :: a -> LabelStates -> FileName -> (LabelStates, ASMCode)

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
  toASM (MemCMD PUSH ARGUMENT x) sts _ =
    (sts,
      let xBs = toByteString' x <> "\n" in
          "    //push argument " <> xBs
      <>  "    @" <> xBs
      <>  "    D=A\n"
      <>  "    @ARG\n"
      <>  "    A=D+M\n"
      <>  "    D=M\n"
      <>  "    @SP\n"
      <>  "    A=M\n"
      <>  "    M=D\n"
      <>  "    @SP\n"
      <>  "    M=M+1\n")

  toASM (MemCMD POP  ARGUMENT x) sts _ =
    (sts,
      let xBs = toByteString' x <> "\n" in
          "    //pop argument " <> xBs
      <>  "    @" <> xBs
      <>  "    D=A\n"
      <>  "    @ARG\n"
      <>  "    D=D+M\n"
      <>  "    @R13\n"
      <>  "    M=D\n"
      <>  "    @SP\n"
      <>  "    M=M-1\n"
      <>  "    A=M\n"
      <>  "    D=M\n"
      <>  "    @R13\n"
      <>  "    A=M\n"
      <>  "    M=D\n")

  toASM (MemCMD PUSH LOCAL    x) sts _ =
    (sts,
      let xBs = toByteString' x <> "\n" in
          "    //push local " <> xBs
      <>  "    @" <> xBs
      <>  "    D=A\n"
      <>  "    @LCL\n"
      <>  "    A=D+M\n"
      <>  "    D=M\n"
      <>  "    @SP\n"
      <>  "    A=M\n"
      <>  "    M=D\n"
      <>  "    @SP\n"
      <>  "    M=M+1\n")

  toASM (MemCMD POP  LOCAL    x) sts _ =
    (sts,
      let xBs = toByteString' x <> "\n" in
          "    //pop local " <> xBs
      <>  "    @" <> xBs
      <>  "    D=A\n"
      <>  "    @LCL\n"
      <>  "    D=D+M\n"
      <>  "    @R13\n"
      <>  "    M=D\n"
      <>  "    @SP\n"
      <>  "    M=M-1\n"
      <>  "    A=M\n"
      <>  "    D=M\n"
      <>  "    @R13\n"
      <>  "    A=M\n"
      <>  "    M=D\n")

  toASM (MemCMD PUSH STATIC   x) sts fn =
    (sts, let xBs = toByteString' x <> "\n" 
              stcVar = fn <> toByteString' x <> "\n" in
          "    //push static " <> xBs
      <>  "    @" <> stcVar
      <>  "    D=M\n"
      <>  "    @SP\n"
      <>  "    A=M\n"
      <>  "    M=D\n"
      <>  "    @SP\n"
      <>  "    M=M+1\n")

  toASM (MemCMD POP  STATIC   x) sts fn =
    (sts, let xBs = toByteString' x <> "\n"
              stcVar = fn <> toByteString' x <> "\n" in
          "    //pop static " <> xBs
      <>  "    @SP\n"
      <>  "    M=M-1\n"
      <>  "    A=M\n"
      <>  "    D=M\n"
      <>  "    @" <> stcVar
      <>  "    M=D\n")

  toASM (MemCMD PUSH CONSTANT x) sts _ =
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

  toASM (MemCMD PUSH THIS     x) sts _ =
    (sts,
      let xBs = toByteString' x <> "\n" in
          "    //push this " <> xBs
      <>  "    @" <> xBs
      <>  "    D=A\n"
      <>  "    @THIS\n"
      <>  "    A=D+M\n"
      <>  "    D=M\n"
      <>  "    @SP\n"
      <>  "    A=M\n"
      <>  "    M=D\n"
      <>  "    @SP\n"
      <>  "    M=M+1\n")

  toASM (MemCMD POP  THIS     x) sts _ =
    (sts,
      let xBs = toByteString' x <> "\n" in
          "    //pop this " <> xBs
      <>  "    @" <> xBs
      <>  "    D=A\n"
      <>  "    @THIS\n"
      <>  "    D=D+M\n"
      <>  "    @R13\n"
      <>  "    M=D\n"
      <>  "    @SP\n"
      <>  "    M=M-1\n"
      <>  "    A=M\n"
      <>  "    D=M\n"
      <>  "    @R13\n"
      <>  "    A=M\n"
      <>  "    M=D\n")

  toASM (MemCMD PUSH THAT     x) sts _ =
    (sts,
      let xBs = toByteString' x <> "\n" in
          "    //push that " <> xBs
      <>  "    @" <> xBs
      <>  "    D=A\n"
      <>  "    @THAT\n"
      <>  "    A=D+M\n"
      <>  "    D=M\n"
      <>  "    @SP\n"
      <>  "    A=M\n"
      <>  "    M=D\n"
      <>  "    @SP\n"
      <>  "    M=M+1\n")

  toASM (MemCMD POP  THAT     x) sts _ =
    (sts,
      let xBs = toByteString' x <> "\n" in
          "    //pop that " <> xBs
      <>  "    @" <> xBs
      <>  "    D=A\n"
      <>  "    @THAT\n"
      <>  "    D=D+M\n"
      <>  "    @R13\n"
      <>  "    M=D\n"
      <>  "    @SP\n"
      <>  "    M=M-1\n"
      <>  "    A=M\n"
      <>  "    D=M\n"
      <>  "    @R13\n"
      <>  "    A=M\n"
      <>  "    M=D\n")

  toASM (MemCMD PUSH POINTER  x) sts _ =
    (sts,
      let xBs  = toByteString' x     <> "\n"
          pntr = toByteString' (3+x) <> "\n" in
          "    //push pointer " <> xBs
      <>  "    @" <> pntr
      <>  "    D=M\n"
      <>  "    @SP\n"
      <>  "    A=M\n"
      <>  "    M=D\n"
      <>  "    @SP\n"
      <>  "    M=M+1\n")

  toASM (MemCMD POP  POINTER  x) sts _ =
    (sts,
      let xBs  = toByteString' x     <> "\n"
          pntr = toByteString' (3+x) <> "\n" in
          "    //pop pointer " <> xBs
      <>  "    @SP\n"
      <>  "    M=M-1\n"
      <>  "    A=M\n"
      <>  "    D=M\n"
      <>  "    @" <> pntr
      <>  "    M=D\n")

  toASM (MemCMD PUSH TEMP     x) sts _ =
    (sts,
      let xBs = toByteString' x     <> "\n"
          tmp = toByteString' (5+x) <> "\n" in
          "    //push temp " <> xBs
      <>  "    @" <> tmp
      <>  "    D=M\n"
      <>  "    @SP\n"
      <>  "    A=M\n"
      <>  "    M=D\n"
      <>  "    @SP\n"
      <>  "    M=M+1\n")

  toASM (MemCMD POP  TEMP     x) sts _ =
    (sts,
      let xBs = toByteString' x     <> "\n"
          tmp = toByteString' (5+x) <> "\n" in
          "    //pop temp " <> xBs
      <>  "    @SP\n"
      <>  "    M=M-1\n"
      <>  "    A=M\n"
      <>  "    D=M\n"
      <>  "    @" <> tmp
      <>  "    M=D\n")


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
  toASM ADD sts _ =
    (sts,
        "    //add\n"
    <>  "    @SP\n"
    <>  "    M=M-1\n"
    <>  "    A=M\n"
    <>  "    D=M\n"
    <>  "    A=A-1\n"
    <>  "    M=M+D\n")

  toASM SUB sts _ =
    (sts,
        "    //sub\n"
    <>  "    @SP\n"
    <>  "    M=M-1\n"
    <>  "    A=M\n"
    <>  "    D=M\n"
    <>  "    A=A-1\n"
    <>  "    M=M-D\n")

  toASM NEG sts _ =
    (sts, 
        "    //neg\n"
    <>  "    @SP\n"
    <>  "    A=M-1\n"
    <>  "    M=-M\n")

  toASM EQ_VM (LS e g l) _ =
    (LS (e+1) g l,
      let trueJmp = "TRUE_EQ_" <> toByteString' e
          endJmp  = "END_EQ_"  <> toByteString' e in
            "    //eq\n"
        <>  "    @SP\n"
        <>  "    M=M-1\n"
        <>  "    A=M\n"
        <>  "    D=M\n"
        <>  "    A=A-1\n"
        <>  "    D=D-M\n"
        <>  "    @" <> trueJmp <> "\n"
        <>  "    D;JEQ\n"
        <>  "    @SP\n"
        <>  "    A=M-1\n"
        <>  "    M=0\n"
        <>  "    @" <> endJmp <> "\n"
        <>  "    0;JMP\n"
        <>  "(" <> trueJmp <> ")\n"
        <>  "    @SP\n"
        <>  "    A=M-1\n"
        <>  "    M=-1\n"
        <>  "(" <> endJmp <> ")\n")

  toASM GT_VM (LS e g l) _ =
    (LS e (g+1) l,
      let trueJmp = "TRUE_GT_" <> toByteString' g
          endJmp  = "END_GT_"  <> toByteString' g in
            "    //gt\n"
        <>  "    @SP\n"
        <>  "    M=M-1\n"
        <>  "    A=M\n"
        <>  "    D=M\n"
        <>  "    A=A-1\n"
        <>  "    D=D-M\n"
        <>  "    @" <> trueJmp <> "\n"
        <>  "    D;JLT\n"
        <>  "    @SP\n"
        <>  "    A=M-1\n"
        <>  "    M=0\n"
        <>  "    @" <> endJmp <> "\n"
        <>  "    0;JMP\n"
        <>  "(" <> trueJmp <> ")\n"
        <>  "    @SP\n"
        <>  "    A=M-1\n"
        <>  "    M=-1\n"
        <>  "(" <> endJmp <> ")\n")
  
  toASM LT_VM (LS e g l) _ =
    (LS e g (l+1),
      let trueJmp = "TRUE_LT_" <> toByteString' l
          endJmp  = "END_LT_"  <> toByteString' l in
            "    //lt\n"
        <>  "    @SP\n"
        <>  "    M=M-1\n"
        <>  "    A=M\n"
        <>  "    D=M\n"
        <>  "    A=A-1\n"
        <>  "    D=D-M\n"
        <>  "    @" <> trueJmp <> "\n"
        <>  "    D;JGT\n"
        <>  "    @SP\n"
        <>  "    A=M-1\n"
        <>  "    M=0\n"
        <>  "    @" <> endJmp <> "\n"
        <>  "    0;JMP\n"
        <>  "(" <> trueJmp <> ")\n"
        <>  "    @SP\n"
        <>  "    A=M-1\n"
        <>  "    M=-1\n"
        <>  "(" <> endJmp <> ")\n")

  toASM AND sts _ =
    (sts,
        "    //and\n"
    <>  "    @SP\n"
    <>  "    M=M-1\n"
    <>  "    A=M\n"
    <>  "    D=M\n"
    <>  "    A=A-1\n"
    <>  "    M=D&M\n")

  toASM OR sts _ =
    (sts,
        "    //or\n"
    <>  "    @SP\n"
    <>  "    M=M-1\n"
    <>  "    A=M\n"
    <>  "    D=M\n"
    <>  "    A=A-1\n"
    <>  "    M=D|M\n")

  toASM NOT sts _ =
    (sts,
        "    //not\n"
    <>  "    @SP\n"
    <>  "    A=M-1\n"
    <>  "    M=!M\n")

 
data VMLine =
    AL_VM ArithLogicCommand
  | M_VM  MemoryAccessCommand
  deriving (Eq, Show)

instance ToASMCode VMLine where
  toASM (AL_VM c) = toASM c
  toASM (M_VM  c) = toASM c

type Program = [VMLine]
