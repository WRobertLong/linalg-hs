------------------------------------------------------------
-- Routines to carry out row and column reduction

module RowOperations where

import Matrix
import List

vectorReduction :: Vector -> Vector -> Vector
-- Performs the elementary row operation 
-- [ n, ... ] -> [ kn + q, ... ] -> [ q, ... ]
-- assuming that the first entry of the first vector is nonzero.
vectorReduction firstRow secondRow
  = vectorSum secondRow ( vectorScalarProduct (-k) firstRow )
  where
  k = quot (head secondRow) (head firstRow)

minFirst :: Matrix -> Matrix
-- Move the row whose first entry has the smallest absolute value
-- to be the first row. We must make sure the determinant is
-- multiplied by  -1  when we interchange rows. Also, since a
-- reversal of the row order will be needed at the end, we reverse
-- also at the start to ensure that the determinant is unchanged.
minFirst = reverse . minLast . reverse
  where
  minLast [ ] = [ ]
  minLast [row] = [row]
  minLast (first:second:rest)
    | head first  == 0 = first : (minLast (second:rest))
    | head second == 0 || abs (head first) < abs (head second)
        = (vectorScalarProduct (-1) second) : (minLast (first:rest))
    | otherwise        = first : (minLast (second:rest))

reduceUsingFirstRow :: Matrix -> Matrix
reduceUsingFirstRow [ ] = [ ]
reduceUsingFirstRow m
  | head (head m) == 0 = m
  | otherwise = (head m) : [ vectorReduction (head m) row | row <- tail m ]

reduceFirstColumn :: Matrix -> Matrix
-- Reduce as far as possible in the first column.
reduceFirstColumn m
  | onceReduced == m = m
  | otherwise = reduceFirstColumn (onceReduced)
  where
  onceReduced = reduceUsingFirstRow (minFirst m)

tailBlock :: Matrix -> Matrix
-- A utility to pick out the lower right (n-1)-by-(n-1) block.
tailBlock m = [ tail row | row <- tail m ]

------------------------------------------------------------
-- Calculate the determinant by using row reductions 
-- to reduce to the upper triangular case

det :: Matrix -> Int
det [ ]   = error "empty matrix has no determinant"
det [[n]] = n
det m 
  | numRows m /= numColumns m 
      = error "can only take determinant of square matrix"
  | otherwise 
      = (head (head reducedM) ) * (det ( tailBlock reducedM ))
  where
  reducedM = reduceFirstColumn m

------------------------------------------------------------
-- Calculate the inverse using row and column operations

doubleMatrix :: Matrix -> Matrix -> Matrix
doubleMatrix = zipWith (++)

leftHalf :: Matrix -> Matrix
leftHalf double = [ take size row | row <- double ]
  where
  size = numRows double

rightHalf :: Matrix -> Matrix
rightHalf double = [ drop size row | row <- double ]
  where
  size = numRows double

upperTriangularize :: Matrix -> Matrix
upperTriangularize [ ] = [ ]
upperTriangularize m = [ head ( reduceFirstColumn m ) ] ++
  doubleMatrix (replicate (size-1) [0]) 
    (upperTriangularize ( tailBlock (reduceFirstColumn m)))
  where
  size = numRows m

matrixFlip :: Matrix -> Matrix
matrixFlip m = reverse [ reverse row | row <- m ]

doubleFlip :: Matrix -> Matrix
doubleFlip double 
  = doubleMatrix (matrixFlip ( leftHalf double )) ( matrixFlip ( rightHalf double ))

reduceLowerTriangular :: Matrix -> Matrix
reduceLowerTriangular m =
  (take 1 m) ++ doubleMatrix (replicate ((numRows m)-1) [0]) 
  (reduceLowerTriangular (tailBlock (reduceUsingFirstRow m)))

fixMinusRows :: Matrix -> Matrix
fixMinusRows m = [ [ (m!!i)!!i * x | x <- m!!i ] | i <- [0..(numRows m)-1] ]

inverse :: Matrix -> Matrix
inverse m
  | det m /=1 && det m /= (-1) 
      = error "can only take inverse of M if det M = 1 or -1"
  | otherwise = (rightHalf . fixMinusRows . doubleFlip . 
      reduceLowerTriangular . doubleFlip . upperTriangularize ) 
      ( doubleMatrix m ( idMatrix (numRows m) ) )

------------------------------------------------------------
-- For Smith Normal Form, we reduce the first row and the first
-- column until  m  is in  1 + (n-1)  block form, then use recursion 
-- to peel off the diagonal entries

reduce' :: Matrix -> Matrix
reduce' = transpose . reduceFirstColumn . transpose . reduceFirstColumn

reduce :: Matrix -> Matrix
-- When reduce' no longer decreases the (1,1) entry, the (1,1) entry 
-- divides the rest of the first column, and the rest of the first row 
-- is zero. So we just need one more reduction using the first row.
reduce m
  | abs ( head (head (reduce' m)) ) == abs ( head (head m ) )
      = reduceUsingFirstRow m
  | otherwise        = reduce ( reduce' m )

smithDiagonalEntries :: Matrix -> [Int]
smithDiagonalEntries [ ] = [ ]
smithDiagonalEntries m 
  | numColumns m == 1 =  [ abs ( head ( head (reduce m ) ) ) ]
  | otherwise         = ( abs ( head ( head (reduce m ) ) ) ) :
    (smithDiagonalEntries (tailBlock (reduce m ) ) )

-- In Smith Normal Form, each diagonal entry divides the next one.
-- Achieve this by ordering and "combing", 
-- i. e. replacing [a,b] by [ lcm a b, gcd a b ], as far as possible

comb :: [Int] -> [Int]
comb [ ] = [ ]
comb [n] = [n]
comb (m:n:rest) = (gcd m n) : (comb (lcm m n: rest))

abelianNormalForm :: [Int] -> [Int]
-- Interpreting a list as a product of cyclic abelian groups,
-- rearrange it so that the order of each factor divides the 
-- order of the next one. 
abelianNormalForm list
  | nonzeros == comb nonzeros = nonzeros ++ zeros
  | otherwise = abelianNormalForm ((comb nonzeros) ++ zeros)
  where
  zeros = [ x | x <- list, x == 0 ]
  nonzeros = [ x | x <- list, x /= 0 ]

smithForm :: Matrix -> Matrix
smithForm m = zipWith kthRow entries [1..(numRows m)] ++ extraZeroRows
  where
  kthRow v k = map (\j -> if j == k then v else 0) [1..(numColumns m)]
  entries = abelianNormalForm (smithDiagonalEntries m)
  extraZeroRows = zeroMatrix (numRows m - numColumns m) (numColumns m)

