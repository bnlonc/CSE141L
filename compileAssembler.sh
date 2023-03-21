#!/bin/bash
cd assembler/Assembler
output=$(stack build)
status=$? 
echo $output
#if [ $(stack build) ]
if [ $status -ne 0 ] 
then 
  echo "stack build failed: exited with status $status" 
  exit 1
fi 
echo "stack build succeeded"
cp "$(stack path --local-install-root)/bin/Assembler-exe" ../../Assembler