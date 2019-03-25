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

m :: Int
m = 9

h :: Int
h = m * 2 - 1

a :: Int -> [[Int]]
a x = rowIter 0 []
  where
    rowIter :: Int -> [[Int]] -> [[Int]]
    rowIter y matrix
      | x == y = matrix
      | otherwise = rowIter (y + 1) matrix ++ [aRow x]

aRow :: Int -> [Int]
aRow x = rowIter 0
  where
    rowIter :: Int -> [Int]
    rowIter y
      | x == y = replicate y 0
      | otherwise = rowIter (y + 1)

setRowMatrix :: Int -> Int -> [Int] -> [Int]
setRowMatrix n newElement xs = take n xs ++ [newElement] ++ drop (n + 1) xs

setMatrix :: (Int, Int) -> Int -> [[Int]] -> [[Int]]
setMatrix (a, b) newElement xs = take a xs ++ [setRowMatrix b newElement (matrixIndex a xs)] ++ drop (a + 1) xs

matrixIndex :: Int -> [[Int]] -> [Int]
matrixIndex i matrix = matrix !! i

t :: [[Int]]
t = [[1, 0], [1, -1], [0, -1], [ -1, -1], [ -1, 0], [ -1, 1], [0, 1], [1, 1]]

tIndex :: Int -> [Int]
tIndex c = matrixIndex c t

g :: Int -> Int
g = floor . sqrt . fromIntegral

r :: Int -> Int
r n = quot (g n + g n `mod` 2) 2

q :: Int -> Int
q n = r n * r n * 4

d :: Int -> Int
d n = n - q n

j :: Int -> Int
j n
  | n <= q n - 2 * r n = d n + 3 * r n
  | n <= q n = r n
  | n <= q n + 2 * r n = r n - d n
  | otherwise = -1 * r n

k :: Int -> Int
k n
  | n <= q n - 2 * r n = r n
  | n <= q n = -1 * d n - r n
  | n <= q n + 2 * r n = -1 * r n
  | otherwise = d n - 3 * r n

j' :: Int -> Int
j' n = j n + m

k' :: Int -> Int
k' n = k n + m

v :: Int -> [Int]
v n = [j' n - 1, k' n - 1]

v' :: Int -> [Int] -> [Int]
v' c x = map sumTuple (vZip c x)

vZip :: Int -> [Int] -> [(Int, Int)]
vZip c x = zip x (tIndex c)

sumTuple :: (Int, Int) -> Int
sumTuple (a, b) = a + b

s :: Int -> [[Int]] -> [Int] -> Int
s value matrix vector = value + matrix !! head vector !! (vector !! 1)

firstsLoop :: Int -> Int -> [[Int]] -> [[Int]]
firstsLoop index terminal matrix
  | index == terminal = matrix
  | otherwise = firstsLoop (index + 1) terminal (firstLoopSolve index matrix)

secondLoopSolve :: [[Int]] -> Int -> Int
secondLoopSolve matrix n = loopIter 0 1
  where
    loopIter :: Int -> Int -> Int
    loopIter previous index
      | index == 9 = previous
      | otherwise = loopIter (previous + (matrix !! head (v' (index - 1) (v n)) !! (v' (index - 1) (v n) !! 1))) (index + 1)

firstLoopSolve :: Int -> [[Int]] -> [[Int]]
firstLoopSolve n matrix = setMatrix (j' n - 1, k' n - 1) (secondLoopSolve matrix n) matrix

loopTerminator :: Int
loopTerminator = (h - 2) * (h - 2) - 1

getFirstLarger :: [[Int]] -> Int
getFirstLarger matrix = largerIter 1
  where
    largerIter :: Int -> Int
    largerIter index
      | matrix !! j' index !! k' index >= input = matrix !! j' index !! k' index
      | otherwise = largerIter (index + 1)

part2Solve :: [[Int]]
part2Solve = firstsLoop 1 loopTerminator (setMatrix (m - 1, m - 1) 1 (a h))

part1 = print $ solve input

part2 = print $ getFirstLarger part2Solve

--{
--  m=5;
--  h=2*m-1;
--  A=matrix(h, h);
--  print1(A[m, m]=1, ", ");
--  T=[[1, 0], [1, -1], [0, -1], [ -1, -1], [ -1, 0], [ -1, 1], [0, 1], [1, 1]];
--  for(n=1, (h-2)^2-1, g=sqrtint(n);
--  r=(g+g%2)\2;
--  q=4*r^2;
--  d=n-q;
--  if(n<=q-2*r, j=d+3*r; k=r, if(n<=q, j=r; k=-d-r, if(n<=q+2*r, j=r-d; k=-r, j=-r; k=d-3*r)));
--  j=j+m; k=k+m; s=0;
--  for(c=1, 8, v=[j, k]; v+=T[c]; s=s+A[v[1], v[2]]);
--  A[j, k]=s; print1(s, ", "))
--}
