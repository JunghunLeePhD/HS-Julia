module Polynomial where

import Data.Complex 
import Data.List (dropWhileEnd) 

type Poly = [Complex Double]

eval :: Poly -> (Complex Double -> Complex Double)
eval cpoly z = sum $ zipWith (*) cpoly zPowers
    where
        zPowers = iterate (*z) 1 :: [Complx Double]

isComplexZero :: Complex Double -> Bool
isComplexZero c = magnitude c < epsilon
    where 
        epsilon :: Double
        epsilon = 1.0e-12

isPolyZero :: Poly -> Bool
isPolyZero cpoly = all isComplexZero cpoly 

normalize :: Poly -> Poly
normalize poly = dropWhileEnd isComplexZero poly

degree :: Poly -> Int
degree cpoly = 
    let 
        normalizedPoly = normalize cpoly
    in 
        case normalizedPoly of
            [] -> 0          
            xs -> length xs - 1