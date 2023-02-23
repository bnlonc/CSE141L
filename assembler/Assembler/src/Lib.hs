module Lib
    ( assemble
    ) where

import Data.Char(intToDigit)
import Data.List

type MachineCode            = String
type InstrWord              = String 
type Instruction            = (InstrWord, InstrWord, InstrWord, InstrWord)

assemble :: String -> MachineCode
assemble instrs = intercalate "\n" (map encodeInstruction (resolveMacros (parseInstructions (instrs))))

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
    
resolveInstr :: Instruction -> [Instruction]
resolveInstr (name,     arg1,  arg2,    n)  | (any (name ==) ["RShift", "LShift", "RRotate", "DPRSh", "DPLSh"]) = unroll (name, arg1, arg2, n)
resolveInstr ("Zero",   reg,   _,       _)  = [("XOR", reg, reg, "")]
resolveInstr ("Set",    reg,   value,   _)  = [("XOR", reg, reg, ""), ("AddI", reg, value, "")]
resolveInstr ("Swap",   reg1,  reg2,    _)  = [("XOR", reg1, reg2, ""), ("XOR", reg2, reg1, ""), ("XOR", reg1, reg2, "")]
resolveInstr instr                          = [instr]

unroll :: Instruction -> [Instruction]
unroll (name, reg1, reg2, count)    | any (name ==) ["DPRSh", "DPLSh"]        = expandDoublePrecision ( take (read count::Int) (repeat (name, reg1, reg2, "")) )
    where 
        expandDoublePrecision :: [Instruction] -> [Instruction]
        expandDoublePrecision [] = []
        expandDoublePrecision ((name, reg1, reg2, _):ls)                      = (companionShiftName name, reg1, "", ""):(name, reg2, "", ""):(expandDoublePrecision ls)
            where 
                companionShiftName :: String -> String 
                companionShiftName "DPRSh" = "RShift"
                companionShiftName _       = "LShift"
unroll (name, reg, count, _)                                                  = take (read count::Int) (repeat (name, reg, "", ""))


encodeInstruction :: Instruction -> MachineCode
encodeInstruction (name, reg1, reg2, _) | name == "Add"     = "000" ++ (getRegAddr reg1) ++ (getRegAddr reg2)
                                        | name == "Xor"     = "001" ++ (getRegAddr reg1) ++ (getRegAddr reg2)
                                        | name == "And"     = "010" ++ (getRegAddr reg1) ++ (getRegAddr reg2)
encodeInstruction (name, reg1, reg2, _) | name == "Load"    = "011" ++ (getTruncRegAddr reg1) ++ ('0':(getRegAddr reg2))
                                        | name == "Store"   = "011" ++ (getTruncRegAddr reg1) ++ ('1':(getRegAddr reg2))
encodeInstruction (name, reg, imm, _)   | name == "AddI"    = "100" ++ ((getTruncRegAddr reg)) ++ (getImm imm)
encodeInstruction (name, reg, _, _)     | name == "LShift"  = "101" ++ (getRegAddr reg) ++ "000"
                                        | name == "RShift"  = "101" ++ (getRegAddr reg) ++ "010"
                                        | name == "Rotate"  = "101" ++ (getRegAddr reg) ++ "011"
                                        | name == "DPLSh"   = "101" ++ (getRegAddr reg) ++ "100"
                                        | name == "DPRSh"   = "101" ++ (getRegAddr reg) ++ "110"
encodeInstruction (name, mode, reg, _)  | name == "Branch"  = "110" ++ (getRegAddr reg) ++ (getBranchMode mode)
                                        | name == "Parity"  = "111" ++ (getRegAddr reg) ++ (getParityMode mode)

getParityMode :: String -> String
getParityMode "CalcP0"      = "000"
getParityMode "CalcP1"      = "001"
getParityMode "CalcP2"      = "010"
getParityMode "CalcP4"      = "011"
getParityMode "CalcP8"      = "100"
getParityMode "PackLSW"     = "101"
getParityMode "PackMSW"     = "110"
getParityMode "UnpackLSW"   = "111"

getBranchMode :: String -> String 
getBranchMode "IfZeroReg"           = "000"
getBranchMode "IfZeroAbs"           = "001"
getBranchMode "IfNegativeReg"       = "010"
getBranchMode "IfNegativeAbs"       = "011"
getBranchMode "IfNotZeroReg"        = "100"
getBranchMode "IfNotZeroAbs"        = "101"
getBranchMode "IfNotNegativeReg"    = "110"
getBranchMode "IfNotNegativeAbs"    = "111"

getImm :: String -> String
getImm n = getImmHelper (read n::Int)
    where 
        getImmHelper n | n >= 0 = '0':(getRegAddrHelper n)
                       | otherwise = bin (16 + n) 

getTruncRegAddr :: String -> String 
getTruncRegAddr str = tail (getRegAddr str)

getRegAddr :: String -> String 
getRegAddr (r:s:_) = getRegAddrHelper (read [s]::Int)
getRegAddr _       = ""

getRegAddrHelper :: Int -> String
getRegAddrHelper n  | n <= 1 = '0':'0':(bin n)
                    | n <= 3 = '0':(bin n)
                    | otherwise = (bin n)

bin :: Int -> String
bin = reverse . go
    where go 0 = "0"
          go 1 = "1"
          go n = intToDigit (mod n 2) : go (div n 2)