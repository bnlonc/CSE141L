module Main (main) where

import Lib

main :: IO ()
main = do
  filecontent <- readFile "assembly_code.txt"
  putStrLn (assemble filecontent)