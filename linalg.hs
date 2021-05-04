-- Linear algebra in Haskell
--
-- Robert Long
--

import Data.List

type Vector = [Float]
type Matrix = [Vector]

m1, m2, m3 :: Matrix
m1 = [[1.0,2.0], [3.0,4.0]]
m2 = [[1.0,2.0,3.0],[4.0,5.0,6.0],[7.0,8.0,9.0]]
m3 = [[1.0,2.0],[3.0,4.0],[5.0,6.0]]

numRows :: Matrix -> Int
numRows = length

numcols :: Matrix -> Int
numcols = length . head

zeroVector :: Int -> Vector
zeroVector n = replicate n 0.0

vectorScalarProduct :: Float -> Vector -> Vector
vectorScalarProduct n vec = [ n * x | x <- vec ]

matrixScalarProduct :: Float -> Matrix -> Matrix
matrixScalarProduct n m = [ vectorScalarProduct n row | row <- m ]

vectorSum :: Vector -> Vector -> Vector
vectorSum = zipWith (+)

matrixSum :: Matrix -> Matrix -> Matrix
matrixSum = zipWith vectorSum

dotProduct :: Vector -> Vector -> Float
dotProduct v w = sum ( zipWith (*) v w )

matrixProduct :: Matrix -> Matrix -> Matrix
matrixProduct m n = [ map (dotProduct row) (transpose n) | row <- m ]

diag :: Matrix -> Maybe Vector
diag m 
    | numRows m /= numcols m = Nothing
    | otherwise = Just (zipWith (!!) m [0..])

trace :: Matrix -> Maybe Float
trace m 
    | numRows m /= numcols m = Nothing
    | otherwise = Just $ sum (zipWith (!!) m [0..])

cut :: [a] -> Int -> [a]
cut [ ] n = [ ]
cut xs n
    | n < 1 || n > (length xs) = xs
    | otherwise = (take (n-1) xs) ++ drop n xs

remove :: Matrix -> Int -> Int -> Matrix
remove m i j
    | m == [ ] || i < 1 || i > numRows m || j < 1 || j > numcols m = error "remove: (i,j) out of range"
    | otherwise = transpose ( cut (transpose ( cut m i ) ) j )
        
det :: Matrix -> Float
det [] = error "determinant: 0-by-0 matrix"
det [[n]] = n
det m 
    = sum [ (-1)^ (j+1) * (head m)!!(j-1) * det (remove m 1 j) |
      j <- [1..(numcols m) ] ]


cofactor :: Matrix -> Int -> Int -> Float
cofactor m i j = (-1.0)^ (i+j) * det (remove m i j)
      
cofactorMatrix :: Matrix -> Matrix
cofactorMatrix m =
    [ [ (cofactor m i j) | j <- [1..n] ] | i <- [1..n] ]
    where
    n = length m

inverse :: Matrix -> Matrix
inverse m = transpose [ [ quot x (det m) | x <- row ] |
    row <- (cofactorMatrix m) ]