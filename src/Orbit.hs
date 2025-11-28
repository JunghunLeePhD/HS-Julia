module Orbit where

import Data.Complex
import Data.Monoid (Endo(appEndo))
import Polynomial 

orbit :: Poly (Complex Double) -> Int -> Complex Double -> [Complex Double]
orbit p maxIter startZ = take maxIter $ iterate f startZ
    where
        f = appEndo (toEndo p)

escapeRadius :: Poly (Complex Double) -> Double
escapeRadius (Poly []) = 2
escapeRadius (Poly [_]) = 2
escapeRadius (Poly coeffs) = 
    let 
        mags = magnitude <$> coeffs
        leading_term:others = reverse mags
        bound = 
            if leading_term == 0 then 2.0 
            else 1.0 + maximum ((/ leading_term) <$> others)
    in 
        max 2.0 bound

escapeTime :: Poly (Complex Double) -> Int -> Complex Double -> Int
escapeTime p maxIter startZ = go 0 startZ
  where
    f = appEndo (toEndo p)
    go :: Int -> Complex Double -> Int
    go i z
        | i >= maxIter = maxIter
        | magnitude z > 2.0   = i
        | otherwise           = go (i + 1) (f z)

orbitWithCount :: Poly (Complex Double) -> Int -> Complex Double -> (Maybe (Complex Double), Int)
orbitWithCount p maxIter startZ = go (Just startZ) 0
    where
        f = appEndo (toEndo p)
        go val count 
            | count >= maxIter = (val, count)
            | otherwise = case val of
                Just z | magnitude z <= 4 -> go (Just (f z)) (count + 1)
                _              -> (Nothing, count)
