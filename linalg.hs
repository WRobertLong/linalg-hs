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
m4 = [[3,0,2],[2,0,-2],[0,1,1]]

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
inverse m = transpose [ [ x / (det m) | x <- row ] |
    row <- (cofactorMatrix m) ]

elemMatrix :: Int -> Int -> Int -> Float -> Matrix
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

eProduct :: Int -> [(Int, Int, Float)] -> Matrix
-- eProduct n [(Int,Int,Float)] is the product of the elementary matrices
eProduct n [ ] = idMatrix n
eProduct n ((i,j,value):rest) = matrixProduct ( elemMatrix n i j value)
  (eProduct n rest)

minSize :: [(Int,Int,Float)] -> Int
-- smallest size of matrix for which all elementary matrices are defined
minSize list = maximum (concat [ [i,j] | (i,j,value) <- list ] )

checkInverse :: [(Int,Int,Float)] -> String
checkInverse list =
  "\n M = " ++ (show m) ++ "\nInverse(M) = " ++ (show (inverse m)) ++
  if matrixProduct m (inverse m) == idMatrix n then "\nOK.\n" else "\nError.\n"
  where
  m = eProduct n list
  n = minSize list

list1 :: [(Int,Int,Float)]
list1 = [(1,2,1.0), (1,3,-1.0), (1,2,1.0), (3,2,-2.0), (3,1,-3.0)]
list2 :: [(Int,Int,Float)]
list2 = [(1,2,4.0), (4,2,-1.0), (4,1,-2.0), (4,1,1.0), (1,3,-3.0), (2,3,2.0),
  (1,2,2.0), (1,4,-3.0), (1,3,-1.0), (3,2,-1.0), (3,1,-1.0)]
  
test :: IO()
test = putStr (( checkInverse list1 ) ++ ( checkInverse list2 ) )