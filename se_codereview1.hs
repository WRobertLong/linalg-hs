-- For submission to codereview.SE
--
-- 
import Data.List
import Text.Printf 

type Vector = [Double]
type Matrix = [Vector]

-- number of rows of a Matrix
nrow :: Matrix -> Int
nrow = length

-- number of columns of a Matrix
ncol :: Matrix -> Int
ncol = length . head

-- the product of a Vector multiplied by a scalar
vectorScalarProduct :: Double -> Vector -> Vector
vectorScalarProduct n vec = [ n * x | x <- vec ]

-- the prodict of a Matrix multiplied by a scalar
matrixScalarProduct :: Double -> Matrix -> Matrix
matrixScalarProduct n m = [ vectorScalarProduct n row | row <- m ]

-- the sum of two vectors
vectorSum :: Vector -> Vector -> Vector
vectorSum = zipWith (+)

-- negate a Vector
negV :: Vector -> Vector
negV = map negate

-- the dot product of two vectors
dotProduct :: Vector -> Vector -> Double
dotProduct v w
    | length v /= length w = error "dotProduct: Vectors should be of equal length"
    | otherwise = sum ( zipWith (*) v w )

-- product of matrix and vector m %*% v
mvProduct :: Matrix -> Vector -> Vector
mvProduct m v
    | ncol m /= length v = error "mvProduct: incompatible dimensions"
    | otherwise = [ dotProduct row v | row <- m]

-- matrix multiplication
matrixProduct :: Matrix -> Matrix -> Matrix
matrixProduct m n
    | ncol m /= nrow n = error "matrixProduct: incompatible dimensions"
    | otherwise = [ map (dotProduct row) (transpose n) | row <- m ]

-- diagnonal entries of a square matrix
diag :: Matrix -> Vector
diag m 
    | nrow m /= ncol m = error "diag: undefined for a non-square Matrox"
    | otherwise = (zipWith (!!) m [0..])

-- cut out an element of a Vector or a row of a Matrix
cut :: [a] -> Int -> [a]
cut [] n = []
cut xs n
    | n < 1 || n > (length xs) = xs
    | otherwise = (take (n-1) xs) ++ drop n xs

-- remove all entries in the same row and column as the i,j the entry
remove :: Matrix -> Int -> Int -> Matrix
remove m i j
    | m == [] || i < 1 || i > nrow m || j < 1 || j > ncol m = error "remove: (i,j) out of range"
    | otherwise = transpose $ cut (transpose $ cut m i ) j
            
-- determinant of a square matrix
det :: Matrix -> Double
det [] = error "det: determinant not defined for a 0 matrix"
det [[n]] = n
det m 
    = sum [ (-1)^ (j+1) * (head m)!!(j-1) * det (remove m 1 j) |
      j <- [1..(ncol m) ] ]

-- Cofactor i,j
cofactor :: Matrix -> Int -> Int -> Double
cofactor m i j = (-1.0)^ (i+j) * det (remove m i j)
          
-- Cofactor Matrix
cofactorMatrix :: Matrix -> Matrix
cofactorMatrix m =
    [ [ (cofactor m i j) | j <- [1..n] ] | i <- [1..n] ]
    where
      n = length m

-- inverse of a square Matrix
inverse :: Matrix -> Matrix
inverse m = transpose [ [ x / (det m) | x <- cofm ] |
    cofm <- (cofactorMatrix m) ]
      
-- Statistical functions

-- sample mean
mean :: [Double] -> Double
mean [] = error "mean of empty Vector is not defined"
mean xs = sum xs / fromIntegral (length xs)

mean1 :: (Real a, Fractional b) => [a] -> b
mean1 xs = realToFrac (sum xs) / genericLength xs

foo :: [Int]
foo = [1,2,3]

-- sample variance
var :: Vector -> Double
var [] = error "variance of empty Vector is not defined"
var xs = sum (map (^2) (map (subtract (mean xs)) xs)) / fromIntegral (length xs - 1)

--  multiple regression toy data

x :: Matrix
x = [[1, -0.6264538, -0.8204684],
     [1,  0.1836433,  0.4874291],
     [1, -0.8356286,  0.7383247],
     [1,  1.5952808,  0.5757814],
     [1,  0.3295078, -0.3053884]]

y :: Vector     
y = [0.06485897,  1.06091561, -0.71854449, -0.04363773,  1.14905030]

n = nrow x

p = ncol x - 1

-- inverse of XX' needed for the hat matrix
inverse_X_X' = inverse $ matrixProduct (transpose x) x

-- the hat hatrix
hat = matrixProduct inverse_X_X' (transpose x)

-- the regression coefficient estimates
betas = mvProduct hat y

-- fitted values
fitted = mvProduct x betas

-- residuals
res = vectorSum y $ negV fitted

-- Total sum of squares
ssto = sum $ map (^2) $ map (subtract $ mean y) y

-- Sum of squared errors
sse = sum $ map (^2) res

-- Regression sum of squares
ssr = ssto - sse

-- mean squared error
mse = sse / fromIntegral (n - p - 1)

-- regression mean square
msr = ssr / fromIntegral (p)

-- F statistic for the regression
f = msr / mse
-- on p and n-p-1 degrees of freedom

-- standard error of regression coefficients
se_coef = map (sqrt) $ diag $ matrixScalarProduct mse inverse_X_X'

-- r-squared
r2 = 1 - (sse / ssto)

-- adjusted r-squared
r2_adj = 1 - (mse / var y)

-- helper functions for output

vector_to_string :: Vector -> String
vector_to_string xs = unwords $ printf "%.3f" <$> xs

matrix_to_string :: Matrix -> String
matrix_to_string m =
    concat $ intersperse "\n"
        (map (\x -> unwords $ printf "%.3f" <$> (x::[Double])) m )

lm :: IO()
lm = putStr ("Estimates:   " ++ (vector_to_string betas) ++ "\n" ++ 
    "Std. Error:  " ++ (vector_to_string se_coef )++ "\n" ++
    "R-squared:   " ++ printf "%.3f" r2 ++ "  Adj R-sq:  " ++ printf "%.3f" r2_adj ++ "\n" ++
    "F Statistic: " ++ printf "%.3f" f ++ " on " ++ show p ++ " and " ++ show (n - p - 1) ++ " degrees of freedom" ++
    "\n")
