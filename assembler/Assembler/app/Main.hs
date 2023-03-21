module Main (main) where

import Lib

main :: IO ()
main = do
  filecontent1 <- readFile "assemblyCode1.txt"
  filecontent2 <- readFile "assemblyCode2.txt"
  filecontent3 <- readFile "assemblyCode3.txt"

  putStrLn "Resolved assembly for program 1 to:"
  putStrLn (resolve filecontent1)
  putStrLn ""
  putStrLn "Resolved assembly for program 2 to:"
  putStrLn (resolve filecontent2)
  putStrLn ""
  putStrLn "Resolved assembly for program 3 to:"
  putStrLn (resolve filecontent3)

  writeFile "machineCode1.txt" (assemble filecontent1)
  writeFile "machineCode2.txt" (assemble filecontent2)
  writeFile "machineCode3.txt" (assemble filecontent3)