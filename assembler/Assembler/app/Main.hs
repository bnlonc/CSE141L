module Main (main) where

import Lib

main :: IO ()
main = do
  filecontent <- readFile "assembly_code.txt"
  putStrLn ("Resolved assembly to:")
  putStrLn (resolve filecontent)
  putStrLn ("\nAssembled code to:")
  putStrLn (assemble filecontent)