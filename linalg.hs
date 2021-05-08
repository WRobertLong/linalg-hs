-- Statistics in Haskell usinh linear algebra
--
-- Robert Long
--

import Data.List
import Text.Printf 

type Vector = [Double]
type Matrix = [Vector]

-- test matrices
m1, m2, m3, h :: Matrix
m1 = [[1.0,2.0], [3.0,4.0]]
m2 = [[1.0,2.0,3.0],[4.0,5.0,6.0],[7.0,8.0,9.0]]
m3 = [[1.0,2.0],[3.0,4.0],[5.0,6.0]]
m4 = [[3.0,0.0,2.0],[2.0,0.0,-2.0],[0.0,1.0,1.0]]

-- hilbert matrix
h = [[1, 1/2, 1/3, 1/4, 1/5],
   [1/2, 1/3, 1/4, 1/5, 1/6],
   [1/3, 1/4, 1/5, 1/6, 1/7],
   [1/4, 1/5, 1/6, 1/7, 1/8],
   [1/5, 1/6, 1/7, 1/8, 1/9]]
   

   -- Linear algebra functions

-- number of rows of a Matrix
nrow :: Matrix -> Int
nrow = length

-- number of columns of a Matrix
ncol :: Matrix -> Int
ncol = length . head

-- a Vector containing only zeros of length n
zeroVector :: Int -> Vector
zeroVector n = replicate n 0.0

-- the product of a Vector multiplied by a scalar
vectorScalarProduct :: Double -> Vector -> Vector
vectorScalarProduct n vec = [ n * x | x <- vec ]

-- the prodict of a Matrix multiplied by a scalar
matrixScalarProduct :: Double -> Matrix -> Matrix
matrixScalarProduct n m = [ vectorScalarProduct n row | row <- m ]

-- the sum of two vectors where we can pass (+) or (-)
vectorSum1 :: (Double -> Double -> Double) -> Vector -> Vector -> Vector
vectorSum1 = zipWith

-- the sum of two vectors
vectorSum :: Vector -> Vector -> Vector
vectorSum = zipWith (+)

-- the sum of two matrices
matrixSum :: Matrix -> Matrix -> Matrix
matrixSum = zipWith vectorSum

-- negate a Vector
negV :: Vector -> Vector
negV = map negate

-- negate a Matrix
negM :: Matrix -> Matrix
negM m = [ negV row | row <- m ]

-- the dot product of two vectors
dotProduct :: Vector -> Vector -> Double
dotProduct v w
    | length v /= length w = error "Vectors should be of equal length to take the dot product"
    | otherwise = sum ( zipWith (*) v w )

-- product of matrix and vector m %*% v
mvProduct :: Matrix -> Vector -> Vector
mvProduct m v
    | ncol m /= length v = error "incompatible dimensions"
    | otherwise = [ dotProduct row v | row <- m]

-- matrix multiplication
-- TODO: make it work where one (or both) is a vector
matrixProduct :: Matrix -> Matrix -> Matrix
-- matrixProduct m n = [ map (dotProduct row) (transpose n) | row <- m ]
matrixProduct m n
    | ncol m /= nrow n = error "incompatible dimensions"
    | otherwise = [ map (dotProduct row) (transpose n) | row <- m ]

-- the centre diagonal of the square matrix
--diag :: Matrix -> Maybe Vector
--diag m 
--    | nrow m /= ncol m = Nothing
--    | otherwise = Just (zipWith (!!) m [0..])

diag :: Matrix -> Vector
diag m 
    | nrow m /= ncol m = error "diag of a non-square matrix is not defined"
    | otherwise = (zipWith (!!) m [0..])

    -- the trace of a square matrix
trace :: Matrix -> Double
trace m 
    | nrow m /= ncol m = error "trace of a non square matrix is not defined"
    | otherwise = sum (zipWith (!!) m [0..])

cut :: [a] -> Int -> [a]
cut [] n = []
cut xs n
    | n < 1 || n > (length xs) = xs
    | otherwise = (take (n-1) xs) ++ drop n xs

remove :: Matrix -> Int -> Int -> Matrix
remove m i j
    | m == [] || i < 1 || i > nrow m || j < 1 || j > ncol m = error "remove: (i,j) out of range"
    | otherwise = transpose ( cut (transpose ( cut m i ) ) j )
        
det :: Matrix -> Double
det [] = error "determinant of a 0 matrix is not defined"
det [[n]] = n
det m 
    = sum [ (-1)^ (j+1) * (head m)!!(j-1) * det (remove m 1 j) |
      j <- [1..(ncol m) ] ]

cofactor :: Matrix -> Int -> Int -> Double
cofactor m i j = (-1.0)^ (i+j) * det (remove m i j)
      
cofactorMatrix :: Matrix -> Matrix
cofactorMatrix m =
    [ [ (cofactor m i j) | j <- [1..n] ] | i <- [1..n] ]
    where
      n = length m

inverse :: Matrix -> Matrix
inverse m = transpose [ [ x / (det m) | x <- cofm ] |
    cofm <- (cofactorMatrix m) ]
  
-- Statistical functions

-- sample mean
mean :: Vector -> Double
mean [] = error "mean of empty Vector is not defined"
mean xs = sum xs / fromIntegral (length xs)

-- sample variance
var :: Vector -> Double
var [] = error "variance of empty Vector is not defined"
var xs = sum (map (^2) (map (subtract (mean xs)) xs)) / fromIntegral (length xs - 1)


-- simple regression check
-- x :: Matrix
-- x = [[1.0, 1.0],[1.0,2.0],[1.0,3.0]]
--
-- y :: Vector
-- y = [0.3735462, 2.1836433, 2.1643714]

--  multiple regression check

x :: Matrix
x = [[1, -0.6264538, -0.8204684],
     [1,  0.1836433,  0.4874291],
     [1, -0.8356286,  0.7383247],
     [1,  1.5952808,  0.5757814],
     [1,  0.3295078, -0.3053884]]

y :: Vector     
y = [0.06485897,  1.06091561, -0.71854449, -0.04363773,  1.14905030]

{-
x :: Matrix
x = [[1, 68.5, 16.7],
     [1, 45.2, 16.8],
     [1, 91.3, 18.2],
     [1, 47.8, 16.3],
     [1, 46.9, 17.3],
     [1, 66.1, 18.2],
     [1, 49.5, 15.9],
     [1, 52.0, 17.2],
     [1, 48.9, 16.6],
     [1, 38.4, 16.0],
     [1, 87.9, 18.3],
     [1, 72.8, 17.1],
     [1, 88.4, 17.4],
     [1, 42.9, 15.8],
     [1, 52.5, 17.8],
     [1, 85.7, 18.4],
     [1, 41.3, 16.5],
     [1, 51.7, 16.3],
     [1, 89.6, 18.1],
     [1, 82.7, 19.1],
     [1, 52.3, 16.0]]

y :: Vector     
y = [174.4, 164.4, 244.2, 154.6, 181.6, 207.5, 152.8, 163.2, 145.4, 137.2, 
     241.9, 191.1, 232.0, 145.3, 161.1, 209.7, 146.4, 144.0, 232.6, 224.1, 166.5]
-}

n = nrow x

p = ncol x - 1

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

-- F statistic for the regresion
f = msr / mse
-- on p and n-p-1 degrees of freedom

-- mean squared error
-- mse = (sum $ map (^2) res ) / (fromIntegral (n - p - 1))

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
lm = putStr ( "Estimates:   " ++ (vector_to_string betas) ++ "\n" ++ 
              "Std. Error:  " ++ (vector_to_string se_coef )++ "\n" ++
              "R-squared:   " ++ printf "%.3f" r2 ++ "  Adj R-sq:  " ++ printf "%.3f" r2_adj ++ "\n" ++
              "F Statistic: " ++ printf "%.3f" f ++ " on " ++ show p ++ " and " ++ show (n - p - 1) ++ " degrees of freedom" ++
              "\n")
