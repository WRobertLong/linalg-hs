-- Linear algebra in Haskell
--
-- Robert Long
--

import Data.List

type Vector = [Float]
type Matrix = [Vector]

m1, m2, m3 :: Matrix
m1 = [[1,2], [3,4]]
m2 = [[1,2,3],[4,5,6],[7,8,9]]
m3 = [[1,2],[3,4],[5,6]]

numRows :: Matrix -> Int
numRows = length

numcols :: Matrix -> Int
numcols = length . head

zeroVector :: Float -> Vector
zeroVector n = replicate n 0.0

vectorScalarProduct :: Int -> Vector -> Vector
vectorScalarProduct n vec = [ n * x | x <- vec ]

matrixScalarProduct :: Int -> Matrix -> Matrix
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

trace :: Matrix -> Maybe Int
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
    
det :: Matrix -> Int
det [ ] = error "determinant: 0-by-0 matrix"
det [[n]] = n
det m 
        = sum [ (-1)^ (j+1) * (head m)!!(j-1) * det (remove m 1 j) |
        j <- [1..(numcols m) ] ]

cofactor :: Matrix -> Int -> Int -> Int
cofactor m i j = (-1)^ (i+j) * det (remove m i j)

cofactorMatrix :: Matrix -> Matrix
cofactorMatrix m =
    [ [ (cofactor m i j) | j <- [1..n] ] | i <- [1..n] ]
    where
    n = length m

inverse :: Matrix -> Matrix
inverse m = transpose [ [ quot x (det m) | x <- row ] |
  row <- (cofactorMatrix m) ]

elemMatrix :: Int -> Int -> Int -> Int -> Matrix
-- elemMatrix n i j v is the n-by-n elementary matrix
-- with v in the (i,j) place
elemMatrix n i j v = [ [ entry row column | column <- [1..n] ] | row <- [1..n] ]
  where
  entry x y
    | x == y = 1
    | x == i && y == j = v
    | otherwise = 0

idMatrix :: Int -> Matrix -- identity matrix
idMatrix n = elemMatrix n 1 1 1

eProduct :: Int -> [(Int,Int,Int)] -> Matrix
-- eProduct n [(Int,Int,Int)] is the product of the elementary matrices
eProduct n [ ] = idMatrix n
eProduct n ((i,j,value):rest) = matrixProduct ( elemMatrix n i j value)
  (eProduct n rest)

minSize :: [(Int,Int,Int)] -> Int
-- smallest size of matrix for which all elementary matrices are defined
minSize list = maximum (concat [ [i,j] | (i,j,value) <- list ] )

checkInverse :: [(Int,Int,Int)] -> String
checkInverse list =
  "\n M = " ++ (show m) ++ "\nInverse(M) = " ++ (show (inverse m)) ++
  if matrixProduct m (inverse m) == idMatrix n then "\nOK.\n" else "\nError.\n"
  where
  m = eProduct n list
  n = minSize list

list1 :: [(Int,Int,Int)]
list1 = [(1,2,1), (1,3,-1), (1,2,1), (3,2,-2), (3,1,-3)]
list2 :: [(Int,Int,Int)]
list2 = [(1,2,4), (4,2,-1), (4,1,-2), (4,1,1), (1,3,-3), (2,3,2),
  (1,2,2), (1,4,-3), (1,3,-1), (3,2,-1), (3,1,-1)]

test :: IO()
test = putStr (( checkInverse list1 ) ++ ( checkInverse list2 ) )