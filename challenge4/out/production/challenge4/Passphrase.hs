module Passphrase where

import System.IO
import Data.List.Split
import Data.List

splitParaphrases :: String -> [String]
splitParaphrases = splitOn "\n"

removeEmptyStrings :: [String] -> [String]
removeEmptyStrings = filter (not . null)

splitWords :: [String] -> [[String]]
splitWords = map (splitOn " ")

testPhrase :: [[String]] -> [Bool]
testPhrase = map allDifferent

sortWords :: [[String]] -> [[String]]
sortWords = map sortWord

sortWord :: [String] -> [String]
sortWord = map sort

countValid :: [Bool] -> Int
countValid list = length (filter (== True) list)

allDifferent :: [String] -> Bool
allDifferent list = length (removeDuplicates list) == length list

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldl (\seen x -> if x `elem` seen
                                      then seen
                                      else seen ++ [x]) []

solvePart1 :: String -> Int
solvePart1 = countValid . testPhrase . splitWords . removeEmptyStrings . splitParaphrases

solvePart2 :: String -> Int
solvePart2 = countValid . testPhrase . sortWords . splitWords . removeEmptyStrings . splitParaphrases

part1 = do
    handle <- openFile "src/input.txt" ReadMode
    contents <- hGetContents handle
    print $ solvePart1 contents
    hClose handle

part2 = do
    handle <- openFile "src/input.txt" ReadMode
    contents <- hGetContents handle
    print $ solvePart2 contents
    hClose handle
