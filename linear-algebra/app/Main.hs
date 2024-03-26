module Main where

import Numeric.LinearAlgebra

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- Basic Matrices
m1 :: Matrix R
m1 = matrix 3 [1 .. 9]

m2 :: Matrix R
m2 = (2 >< 3) [1 ..]

m3 :: Matrix R
m3 = fromRows [vector [1, 2, 3], vector [2, 5, 2], vector [6, -3, 1]]
