{-# LANGUAGE OverloadedStrings #-}

module Model where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Conversion (toByteString')
import System.IO

type FileName = BS.ByteString
type ASMCode  = BS.ByteString

type EqLabel = Integer
type GtLabel = Integer
type LtLabel = Integer
type RetLabel = Integer
data InFunction =
    Inside Label
  | Outside
  deriving (Eq, Show)

data WritingStates = LS EqLabel GtLabel LtLabel RetLabel InFunction Handle
                  deriving (Eq, Show)

-- ===== --
-- Model --
-- This module holds all the data definitions that the parser will parse into.

class ToASMCode a where
  toASM :: a -> WritingStates -> FileName -> (WritingStates, ASMCode)

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

  toASM (MemCMD PUSH STATIC   x) sts fileName =
    (sts, let xBs = toByteString' x <> "\n" 
              stcVar = fileName <> toByteString' x <> "\n" in
          "    //push static " <> xBs
      <>  "    @" <> stcVar
      <>  "    D=M\n"
      <>  "    @SP\n"
      <>  "    A=M\n"
      <>  "    M=D\n"
      <>  "    @SP\n"
      <>  "    M=M+1\n")

  toASM (MemCMD POP  STATIC   x) sts fileName =
    (sts, let xBs = toByteString' x <> "\n"
              stcVar = fileName <> toByteString' x <> "\n" in
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

  toASM EQ_VM (LS e g l r funName h) _ =
    (LS (e+1) g l r funName h,
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

  toASM GT_VM (LS e g l r funName h) _ =
    (LS e (g+1) l r funName h,
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
  
  toASM LT_VM (LS e g l r funName h) _ =
    (LS e g (l+1) r funName h,
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

type Label = BS.ByteString

data ProgramFlowCommand =
    LABEL   Label
  | GOTO    Label
  | IF_GOTO Label
  deriving (Eq, Show)

instance ToASMCode ProgramFlowCommand where
  toASM (LABEL label) sts@(LS e g l r funName h) fileName =
    (sts,
        let label' = case funName of
                      Inside funName -> funName <> "$" <> label
                      Outside        -> label
                      in
        "    //label " <> label <> "\n"
    <>  "(" <> label' <> ")\n")

  toASM (GOTO label) sts@(LS e g l r (Inside funName) h) fileName =
    (sts,
        let labelWithNL = funName <> "$" <> label <> "\n" in
        "    //goto " <> labelWithNL
    <>  "    @" <> labelWithNL
    <>  "    0;JMP\n")

  toASM (IF_GOTO label) sts@(LS e g l r (Inside funName) h) fileName =
    (sts,
        let labelWithNL = funName <> "$" <> label <> "\n" in
        "    //if-goto " <> labelWithNL
    <>  "    @SP\n"
    <>  "    AM=M-1\n"
    <>  "    M=D\n"
    <>  "    @" <> labelWithNL
    <>  "    D;JNE\n")

data FunctionCommand =
    FUN Label Integer
  | CALL Label Integer
  | RETURN
  deriving (Eq, Show)

instance ToASMCode FunctionCommand where
  toASM (FUN label nVars) (LS e g l r funName h) fileName =
    let updateFunName Outside = Inside label
        nPush0 0 = ""
        nPush0 n =     "    @0\n"
                   <>  "    D=A\n"
                   <>  "    @SP\n"
                   <>  "    A=M\n"
                   <>  "    M=D\n"
                   <>  "    @SP\n"
                   <>  "    M=M+1\n"
                   <>  nPush0 (n-1) in --todo improve algo: remove rep by creating internal rec fun.
    (LS e g l r (updateFunName funName) h,
        "    //function " <> label <> " " <> toByteString' nVars <> "\n"
    <>  "(" <> label <> ")\n"--todo add filename here?
    <>  nPush0 nVars)
  
  toASM (CALL funToCall nArgs) (LS e g l r funName h) fileName =
    (LS e g l (r+1) funName h,
        let retAdd = "RETURN_LOCATION_$" <> toByteString' r in
        "    //call " <> funToCall <> " " <> toByteString' nArgs <> "\n"
    <>  "    @" <> retAdd <> "\n"
    <>  "    D=A\n"
    <>  "    @SP\n"
    <>  "    A=M\n"
    <>  "    M=D\n"

    <>  "    @LCL\n"
    <>  "    M=D\n"
    <>  "    @SP\n"
    <>  "    AM=M+1\n"
    <>  "    M=D\n"

    <>  "    @ARG\n"
    <>  "    M=D\n"
    <>  "    @SP\n"
    <>  "    AM=M+1\n"
    <>  "    M=D\n"

    <>  "    @THIS\n"
    <>  "    M=D\n"
    <>  "    @SP\n"
    <>  "    AM=M+1\n"
    <>  "    M=D\n"

    <>  "    @THAT\n"
    <>  "    M=D\n"
    <>  "    @SP\n"
    <>  "    AM=M+1\n"
    <>  "    M=D\n"

    <>  "    @SP\n"
    <>  "    MD=M+1\n"
    <>  "    @LCL\n"
    <>  "    M=D\n"
    
    <>  "    @" <> toByteString' nArgs <> "\n"
    <>  "    D=D-A\n"
    <>  "    @5\n"
    <>  "    D=D-A\n"
    <>  "    @ARG\n"
    <>  "    M=D\n"

    <>  "    @" <> funToCall <> "\n"
    <>  "    0;JMP\n"
    <>  "(" <> retAdd <> ")\n")

  toASM RETURN (LS e g l r funName h) fileName =
    let updateFunName (Inside _) = Outside in
      (LS e g l r (updateFunName funName) h,
          "    //return\n"
      <>  "       //ret: FRAME = LCL\n"
      <>  "    @LCL\n"
      <>  "    D=M\n"
      <>  "    @R5\n"
      <>  "    M=D\n"

      <>  "       //ret: RET = *(FRAME-5)\n"
      <>  "    @5\n"
      <>  "    D=A\n"
      <>  "    @R5\n"
      <>  "    A=M-D\n"
      <>  "    D=M\n"
      <>  "    @R6\n"
      <>  "    M=D\n"

      <>  "       //ret: *ARG = pop()\n"
      <>  "    @SP\n"
      <>  "    M=M-1\n"
      <>  "    A=M\n" -- todo can we use AM=?
      <>  "    D=M\n"
      <>  "    @ARG\n"
      <>  "    A=M\n"
      <>  "    M=D\n"

      <>  "       //ret: SP = ARG+1\n"
      <>  "    @ARG\n"
      <>  "    D=M+1\n"
      <>  "    @SP\n"
      <>  "    M=D\n"

      <>  "       //ret: THAT = *(FRAME-1)\n"
      <>  "    @R5\n"
      <>  "    A=M-1\n"
      <>  "    D=M\n"
      <>  "    @THAT\n"
      <>  "    M=D\n"

      <>  "       //ret: THIS = *(FRAME-2)\n"
      <>  "    @2\n"
      <>  "    D=A\n"
      <>  "    @R5\n"
      <>  "    A=M-D\n"
      <>  "    D=M\n"
      <>  "    @THIS\n"
      <>  "    M=D\n"

      <>  "       //ret: ARG = *(FRAME-3)\n"
      <>  "    @3\n"
      <>  "    D=A\n"
      <>  "    @R5\n"
      <>  "    A=M-D\n"
      <>  "    D=M\n"
      <>  "    @ARG\n"
      <>  "    M=D\n"

      <>  "       //ret: LCL = *(FRAME-4)\n"
      <>  "    @4\n"
      <>  "    D=A\n"
      <>  "    @R5\n"
      <>  "    A=M-D\n"
      <>  "    D=M\n"
      <>  "    @LCL\n"
      <>  "    M=D\n"

      <>  "       //ret: goto RET\n"
      <>  "    @R6\n"
      <>  "    A=M\n"
      <>  "    0;JMP\n")


data VMLine =
    AL_VM ArithLogicCommand
  | M_VM  MemoryAccessCommand
  | P_VM  ProgramFlowCommand
  | F_VM  FunctionCommand
  deriving (Eq, Show)

instance ToASMCode VMLine where
  toASM (AL_VM c) = toASM c
  toASM (M_VM  c) = toASM c
  toASM (P_VM  c) = toASM c
  toASM (F_VM  c) = toASM c

type VMFile = ([VMLine], FileName)
type VMProgram = [VMFile]

type UnparsedVMLine = BS.ByteString
type UnparsedVMFile = ([UnparsedVMLine], FilePath)
type UnparsedVMProgram = [UnparsedVMFile]
