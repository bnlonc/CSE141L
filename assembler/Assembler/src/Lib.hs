module Lib
    ( assemble
    ) where

import Data.Char(intToDigit)
import Data.List

type MachineCode            = String
type InstrWord              = String 
type Instruction            = (InstrWord, InstrWord, InstrWord, InstrWord)

assemble :: String -> MachineCode
assemble instrs = intercalate "\n" (map encodeInstruction (resolveMacros (parseInstructions instrs)))

parseInstructions :: String -> [Instruction]
parseInstructions str = map splitLine (lines str)
    where 
        splitLine :: String -> Instruction 
        splitLine str = packageInstr (words str)
            where 
                packageInstr :: [String] -> Instruction
                packageInstr (name:reg:[])              = (name, reg, "", "")
                packageInstr (name:reg1:reg2:[])        = (name, reg1, reg2, "")
                packageInstr (name:reg1:reg2:count:[])  = (name, reg1, reg2, count)


resolveMacros :: [Instruction] -> [Instruction]
resolveMacros []     = []; 
resolveMacros (l:ls) = (resolveInstr l) ++ (resolveMacros ls)
    where     
        resolveInstr :: Instruction -> [Instruction]
        resolveInstr (name,     arg1,  arg2,    n)  | (any (name ==) ["RShift", "LShift", "Rotate", "DPRSh", "DPLSh"]) = unrollMultiMacro (name, arg1, arg2, n)
        resolveInstr ("Zero",   reg,   _,       _)  = [("Xor", reg, reg, "")]
        resolveInstr ("Set",    reg,   value,   _)  = [("Xor", reg, reg, ""), ("AddI", reg, value, "")]
        resolveInstr ("Swap",   reg1,  reg2,    _)  = [("Xor", reg1, reg2, ""), ("Xor", reg2, reg1, ""), ("Xor", reg1, reg2, "")]
        resolveInstr ("Copy",   reg1,  reg2,    _)  = [("Xor", reg1, reg1, ""), ("Add", reg1, reg2, "")]
        resolveInstr instr                          = [instr]

unrollMultiMacro :: Instruction -> [Instruction]
unrollMultiMacro (name, reg1, reg2, count)    | any (name ==) ["DPRSh", "DPLSh"]    = expandDoublePrecision ( take (read count::Int) (repeat (name, reg1, reg2, "")) )
    where 
        expandDoublePrecision :: [Instruction] -> [Instruction]
        expandDoublePrecision [] = []
        expandDoublePrecision ((name, reg1, reg2, _):ls)                            = (companionShiftName name, reg1, "", ""):(name, reg2, "", ""):(expandDoublePrecision ls)
            where 
                companionShiftName :: String -> String 
                companionShiftName "DPRSh" = "RShift"
                companionShiftName _       = "LShift"
unrollMultiMacro (name, reg, count, _)                                              = take (read count::Int) (repeat (name, reg, "", ""))


encodeInstruction :: Instruction -> MachineCode
encodeInstruction (name, reg1, reg2, _) | name == "Add"     = "000" ++ (encodeRegAddr reg1) ++ (encodeRegAddr reg2)
                                        | name == "Xor"     = "001" ++ (encodeRegAddr reg1) ++ (encodeRegAddr reg2)
                                        | name == "And"     = "010" ++ (encodeRegAddr reg1) ++ (encodeRegAddr reg2)
                                        | name == "Load"    = "011" ++ (encodeTruncRegAddr reg1) ++ ('0':(encodeRegAddr reg2))
                                        | name == "Store"   = "011" ++ (encodeTruncRegAddr reg1) ++ ('1':(encodeRegAddr reg2))
encodeInstruction (name, reg, imm, _)   | name == "AddI"    = "100" ++ (encodeTruncRegAddr reg) ++ (getFourBitBinary imm)
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

getFourBitBinary :: String -> String
getFourBitBinary n = getFourBitBinaryHelper (read n::Int)
    where 
        getFourBitBinaryHelper n    | n >= 0    = '0':(getThreeBitBinary n)
                                    | otherwise = bin (16 + n) 

encodeTruncRegAddr :: String -> String 
encodeTruncRegAddr str = tail (encodeRegAddr str)

encodeRegAddr :: String -> String 
encodeRegAddr (r:s:[]) | r == 'r'  = getThreeBitBinary (read [s]::Int)
encodeRegAddr (r:[])               = getThreeBitBinary (read [r]::Int)
encodeRegAddr _                    = ""

getThreeBitBinary :: Int -> String
getThreeBitBinary n | n <= 1    = '0':'0':(bin n)
                    | n <= 3    = '0':(bin n)
                    | otherwise = (bin n)

bin :: Int -> String
bin = reverse . go
    where go 0 = "0"
          go 1 = "1"
          go n = intToDigit (mod n 2) : go (div n 2)