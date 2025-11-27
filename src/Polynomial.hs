module Polynomial where

import Data.Complex 
import Data.List (dropWhileEnd) 
import Data.Semigroup
import Data.Monoid

newtype Poly = Poly { unPoly :: [Complex Double] }
    deriving (Show, Eq)

instance Semigroup Poly where
    p <> q = compose p q

instance Monoid Poly where
    mempty = Poly [0 :+ 0, 1 :+ 0] 

eval :: Poly -> (Complex Double -> Complex Double)
eval (Poly cpoly) z = sum $ zipWith (*) cpoly zPowers
    where
        zPowers = iterate (*z) 1 :: [Complex Double]

isComplexZero :: Complex Double -> Bool
isComplexZero c = magnitude c < epsilon
    where 
        epsilon :: Double
        epsilon = 1.0e-12

isPolyZero :: Poly -> Bool
isPolyZero (Poly cpoly) = all isComplexZero cpoly 

normalize :: Poly -> Poly
normalize (Poly poly) = Poly (dropWhileEnd isComplexZero poly)

degree :: Poly -> Int
degree cpoly = 
    let 
        (Poly xs) = normalize cpoly
    in 
        case xs of
            [] -> 0          
            _  -> length xs - 1

addLists :: [Complex Double] -> [Complex Double] -> [Complex Double]
addLists [] ys = ys
addLists xs [] = xs
addLists (x:xs) (y:ys) = (x + y) : addLists xs ys

multLists [] _ = []
multLists (p:ps) qs = 
    addLists (map (*p) qs) (0 : multLists ps qs)

addPoly :: Poly -> Poly -> Poly
addPoly (Poly xs) (Poly ys) = Poly (addLists xs ys)

scalePoly :: Complex Double -> Poly -> Poly
scalePoly c (Poly poly) = Poly (map (*c) poly)

multPoly :: Poly -> Poly -> Poly
multPoly (Poly ps) (Poly qs) = Poly (multLists ps qs)

compose :: Poly -> Poly -> Poly
compose (Poly p) q = normalize (foldr step (Poly []) p)
  where
    step coeff acc = addPoly (Poly [coeff]) (multPoly q acc)
