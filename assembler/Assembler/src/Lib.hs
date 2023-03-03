module Lib
    ( 
        assemble,
        unrollMultiAddI
    ) where

import Data.Char(intToDigit)
import Data.List

type MachineCode            = String
type InstrWord              = String 
type Instruction            = (InstrWord, InstrWord, InstrWord, InstrWord)

assemble :: String -> MachineCode
assemble instrs = intercalate "\n" (map encodeInstruction (resolveMacros (parseInstructionString instrs)))

-- Convert the long, \n-delimeted string of instructions to a list of instruction tuples 
parseInstructionString :: String -> [Instruction]
parseInstructionString str = map splitLine (lines str)
    where 
        splitLine :: String -> Instruction 
        splitLine str = packageInstr (words str)
            where 
                packageInstr :: [String] -> Instruction
                packageInstr (name:reg:[])              = (name, reg, "", "")
                packageInstr (name:reg1:reg2:[])        = (name, reg1, reg2, "")
                packageInstr (name:reg1:reg2:count:[])  = (name, reg1, reg2, count)

-- Traverse the list of instructions and resolve every macro to its expanded form in base assembly language 
resolveMacros :: [Instruction] -> [Instruction]
resolveMacros []     = []; 
resolveMacros (l:ls) = (resolveInstr l) ++ (resolveMacros ls)
    where     
        resolveInstr :: Instruction -> [Instruction]
        resolveInstr (name,     arg1,   arg2,   n)  | (any (name ==) ["RShift", "LShift", "Rotate", "DPRSh", "DPLSh"]) = unrollMultiShift (name, arg1, arg2, n)
        resolveInstr ("Zero",   reg,    _,      _)  = [("Xor", reg, reg, "")]
        resolveInstr ("Set",    reg,    value,  _)  = [("Xor", reg, reg, ""), ("AddI", reg, value, "")]
        resolveInstr ("Swap",   reg1,   reg2,   _)  = [("Xor", reg1, reg2, ""), ("Xor", reg2, reg1, ""), ("Xor", reg1, reg2, "")]
        resolveInstr ("Copy",   reg1,   reg2,   _)  = [("Xor", reg1, reg1, ""), ("Add", reg1, reg2, "")]
        resolveInstr ("AddI",   reg,    value,  _)  = unrollMultiAddI reg value
        resolveInstr instr                          = [instr]

unrollMultiAddI :: InstrWord -> InstrWord -> [Instruction]
unrollMultiAddI reg val@(v:vs)  | v == '-'  = reverse (unrollNegative reg (read val::Int))
                                | otherwise = reverse (unrollPositive reg (read val::Int))
    where 
        unrollPositive :: String -> Int -> [Instruction]
        unrollPositive reg val  | val <= 7          = [("AddI", reg, (show val), "")]
                                | mod val 2 == 0    = ("LShift", reg, "", ""):(unrollPositive reg (div val 2))
                                | otherwise         = ("AddI", reg, "7", ""):(unrollPositive reg (val - 7))
        unrollNegative :: String -> Int -> [Instruction] 
        unrollNegative reg val  | val >= -8 = [("AddI", reg, (show val), "")]
                                | otherwise = ("AddI", reg, "-8", ""):(unrollNegative reg (val + 8))

-- Unroll all shift macros to multiple of their base assembly single-shift equivalents
unrollMultiShift :: Instruction -> [Instruction]
unrollMultiShift (name, reg1, reg2, count)    | any (name ==) ["DPRSh", "DPLSh"]    = expandDoublePrecision ( take (read count::Int) (repeat (name, reg1, reg2, "")) )
    where 
        expandDoublePrecision :: [Instruction] -> [Instruction]
        expandDoublePrecision [] = []
        expandDoublePrecision ((name, reg1, reg2, _):ls)                            = (companionShiftName name, reg1, "", ""):(name, reg2, "", ""):(expandDoublePrecision ls)
            where 
                companionShiftName :: String -> String 
                companionShiftName "DPRSh" = "RShift"
                companionShiftName _       = "LShift"
unrollMultiShift (name, reg, count, _)                                              = take (read count::Int) (repeat (name, reg, "", ""))

-- Convert assembly instructions into binary machine code 
encodeInstruction :: Instruction -> MachineCode
encodeInstruction (name, reg1, reg2, _) | name == "Add"     = "000" ++ (encodeRegAddr reg1) ++ (encodeRegAddr reg2)
                                        | name == "Xor"     = "001" ++ (encodeRegAddr reg1) ++ (encodeRegAddr reg2)
                                        | name == "And"     = "010" ++ (encodeRegAddr reg1) ++ (encodeRegAddr reg2)
                                        | name == "Load"    = "011" ++ (encodeTruncRegAddr reg1) ++ ('0':(encodeRegAddr reg2))
                                        | name == "Store"   = "011" ++ (encodeTruncRegAddr reg1) ++ ('1':(encodeRegAddr reg2))
encodeInstruction (name, reg, imm, _)   | name == "AddI"    = "100" ++ (encodeTruncRegAddr reg) ++ (toBinary 4 imm)
encodeInstruction (name, reg, _, _)     | name == "LShift"  = "101" ++ (encodeRegAddr reg) ++ "000"
                                        | name == "RShift"  = "101" ++ (encodeRegAddr reg) ++ "010"
                                        | name == "Rotate"  = "101" ++ (encodeRegAddr reg) ++ "011"
                                        | name == "DPLSh"   = "101" ++ (encodeRegAddr reg) ++ "100"
                                        | name == "DPRSh"   = "101" ++ (encodeRegAddr reg) ++ "110"
encodeInstruction (name, mode, reg, _)  | name == "Branch"  = "110" ++ (encodeRegAddr reg) ++ (encodeBranchMode mode)
                                        | name == "Parity"  = "111" ++ (encodeRegAddr reg) ++ (encodeParityMode mode)

encodeParityMode :: String -> String
encodeParityMode "CalcP0"      = "000"
encodeParityMode "CalcP1"      = "001"
encodeParityMode "CalcP2"      = "010"
encodeParityMode "CalcP4"      = "011"
encodeParityMode "CalcP8"      = "100"
encodeParityMode "PackLSW"     = "101"
encodeParityMode "PackMSW"     = "110"
encodeParityMode "UnpackLSW"   = "111"

encodeBranchMode :: String -> String 
encodeBranchMode "IfZeroReg"           = "000"
encodeBranchMode "IfZeroAbs"           = "001"
encodeBranchMode "IfNegativeReg"       = "010"
encodeBranchMode "IfNegativeAbs"       = "011"
encodeBranchMode "IfNotZeroReg"        = "100"
encodeBranchMode "IfNotZeroAbs"        = "101"
encodeBranchMode "IfNotNegativeReg"    = "110"
encodeBranchMode "IfNotNegativeAbs"    = "111"

-- Convert register names to their truncated binary forms, like "5" to "01"
encodeTruncRegAddr :: String -> String 
encodeTruncRegAddr str = tail (encodeRegAddr str)

-- Convert register names like "r5" or short immediates like "5" into their three-bit binary form, "101"
encodeRegAddr :: String -> String 
encodeRegAddr (r:s:[]) | r == 'r'  = toBinary 3 [s]
encodeRegAddr (r:[])               = toBinary 3 [r]
encodeRegAddr _                    = ""

-- Convert numerical strings into their n-bit binary forms
toBinary :: Int -> String -> String 
toBinary n str = toBinaryHelper n (read str::Int)
    where 
        toBinaryHelper :: Int -> Int -> String 
        toBinaryHelper 0    _   = ""
        toBinaryHelper bits num | num >= (2 ^ (bits - 1)) = '1':(toBinaryHelper (bits - 1) (num - (2 ^ (bits - 1))))
                                | otherwise              = '0':(toBinaryHelper (bits - 1) num)