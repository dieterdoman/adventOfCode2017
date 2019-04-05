module Main where

import Lib
import System.IO
import CpuInterrupts

main :: IO ()
main = do
   handle <- openFile "src/input2.txt" ReadMode
   contents <- hGetContents handle
   print $ solvePart2 contents
   hClose handle
