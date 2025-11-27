module Polynomial where

import Data.Complex 
import Data.List (dropWhileEnd) 

type Poly = [Complex Double]

eval :: Poly -> (Complex Double -> Complex Double)
eval cpoly z = sum $ zipWith (*) cpoly zPowers
    where
        zPowers = iterate (*z) 1 :: [Complex Double]

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


addPoly :: Poly -> Poly -> Poly
addPoly [] ys = ys
addPoly xs [] = xs
addPoly (x:xs) (y:ys) = (x + y) : addPoly xs ys

scalePoly :: Complex Double -> Poly -> Poly
scalePoly c poly = map (*c) poly

multPoly :: Poly -> Poly -> Poly
multPoly [] _ = []
multPoly (p:ps) qs = 
    addPoly (scalePoly p qs) (0 : multPoly ps qs)

compose :: Poly -> Poly -> Poly
compose p q = normalize (foldr step [] p)
  where
    step coeff acc = addPoly [coeff] (multPoly q acc)