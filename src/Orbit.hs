module Orbit where

import Data.Complex
import Data.Monoid (Endo(appEndo))
import Polynomial 
import Ring
import Control.Monad.State -- Import State and StateT
import Control.Monad (guard) -- For the conditional check

orbit :: (Ring a) => Poly a -> Int -> a -> [a]
orbit p maxIter startZ = take maxIter $ iterate f startZ
    where
        f = appEndo (toEndo p)

-- Note: it is inefficient
orbitWithCount :: Poly (Complex Double) -> Int -> Complex Double -> [(Int, Complex Double)]
orbitWithCount p maxIter startZ = take maxIter $ iterate stepFunc initialState
    where
        initialState = (0, startZ)
        stepMonad = do
            (n, z) <- get 
            let w = appEndo (toEndo p) z
            put (n + 1, w)
        stepFunc :: (Int, Complex Double) -> (Int, Complex Double)
        stepFunc = execState stepMonad 

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
escapeTime p maxIter = go 0
  where
    f = appEndo (toEndo p)
    go :: Int -> Complex Double -> Int
    go i z
        | i >= maxIter = maxIter
        | magnitude z > 2.0   = i
        | otherwise           = go (i + 1) (f z)
