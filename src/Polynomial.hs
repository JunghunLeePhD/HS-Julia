module Polynomial where

import Data.Complex 
import Data.List (dropWhileEnd) 

type Poly = [Complex Double]

eval :: Poly -> (Complex Double -> Complex Double)
eval cpoly z = sum $ zipWith (*) cpoly zPowers
    where
        zPowers = iterate (*z) 1 :: [Complx Double]
