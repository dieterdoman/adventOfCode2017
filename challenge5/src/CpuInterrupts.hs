module CpuInterrupts where

import System.IO
import Data.List.Split

splitSteps :: String -> [String]
splitSteps = splitOn "\n"

removeEmptyStrings :: [String] -> [String]
removeEmptyStrings = filter (not . null)

convertToInts :: [String] -> [Int]
convertToInts = map read

getSteps :: [Int] -> Int
getSteps list = stepsIter 0 list 0
  where
    stepsIter :: Int -> [Int] -> Int -> Int
    stepsIter counter steps index
      | index >= length steps = counter
      | otherwise = stepsIter (counter + 1) (increaseStep index steps) (index + steps !! index)

getStepsPart2 :: [Int] -> Int
getStepsPart2 list = stepsIter 0 list 0
  where
    stepsIter :: Int -> [Int] -> Int -> Int
    stepsIter counter steps index
      | index >= length steps = counter
      | otherwise = stepsIter (counter + 1) (increaseStep2 index steps) (index + steps !! index)

increaseStep :: Int -> [Int] -> [Int]
increaseStep n xs = take n xs ++ [xs !! n + 1] ++ drop (n + 1) xs

increaseStep2 :: Int -> [Int] -> [Int]
increaseStep2 n xs
  | xs !! n < 3 = take n xs ++ [xs !! n + 1] ++ drop (n + 1) xs
  | otherwise = take n xs ++ [xs !! n - 1] ++ drop (n + 1) xs

solvePart1 :: String -> Int
solvePart1 = getSteps . convertToInts . removeEmptyStrings . splitSteps

solvePart2 :: String -> Int
solvePart2 = getStepsPart2 . convertToInts . removeEmptyStrings . splitSteps

part1 = do
    handle <- openFile "src/input.txt" ReadMode
    contents <- hGetContents handle
    print $ solvePart1 contents
    hClose handle

part2 = do
    handle <- openFile "src/input2.txt" ReadMode
    contents <- hGetContents handle
    print $ solvePart2 contents
    hClose handle
