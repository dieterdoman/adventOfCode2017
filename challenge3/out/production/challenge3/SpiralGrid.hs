module SpiralGrid where

type InputRingNo = (Int, Int)
type InputRingNoPosition = (Int, Int, Int)
type OffsetRingNo = (Int, Int)

squaredCalc :: Int -> Int
squaredCalc x = (2 * x + 1) * (2 * x + 1)

ringCount :: Int -> Int
ringCount x = smallestRingIter 0
  where
    smallestRingIter :: Int -> Int
    smallestRingIter y
      | x <= squaredCalc y = y
      | otherwise = smallestRingIter (y + 1)

ringNumber :: Int -> InputRingNo
ringNumber x = (x, ringCount x)

position :: InputRingNo -> InputRingNoPosition
position (x, y) = (x, y, squaredCalc y - x)

offset :: InputRingNoPosition -> OffsetRingNo
offset (x, y, z)
  | y == 0 = (y, 0)
  | otherwise = (y, modSwapper (leftSide y z) (rightSide y))

leftSide :: Int -> Int -> Int
leftSide x y = x + y

rightSide :: Int -> Int
rightSide x = quot (squaredCalc x - squaredCalc (x - 1)) 4

modSwapper :: Int -> Int -> Int
modSwapper x y
  | x < y = y `mod` x
  | otherwise = x `mod` y

sum' :: OffsetRingNo -> Int
sum' (x, y) = x + y

solve :: Int -> Int
solve = sum' . offset . position . ringNumber

input :: Int
input = 312051

part1 = print $ solve input
