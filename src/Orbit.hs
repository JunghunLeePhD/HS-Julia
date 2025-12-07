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
orbitWithCount :: (Ring a) => Poly a -> Int -> a -> [(Int, a)]
orbitWithCount p maxIter startZ = take maxIter $ iterate stepFunc initialState
    where
        initialState = (0, startZ)
        stepMonad = do
            (n, z) <- get 
            let w = appEndo (toEndo p) z
            put (n + 1, w)
        stepFunc = execState stepMonad 

escapeRadius :: (NormedRing a) => Poly a -> Double
escapeRadius (Poly coeffs) = 
    let 
        mags = norm <$> coeffs
        revMags = dropWhile (== 0) (reverse mags)
    in 
        case revMags of
            [] -> 2.0 
            [_] -> 2.0 
            (leadingTerm : others) -> 
                let 
                    termRatios = map (/ leadingTerm) others
                    bound = 1.0 + maximum termRatios
                in 
                    max 2.0 bound

escapeTime :: (NormedRing a) => Poly a -> Int -> a -> Int
escapeTime p maxIter = go 0
  where
    bound = escapeRadius p 
    f = appEndo (toEndo p)
    
    go i z
        | i >= maxIter   = maxIter
        | norm z > bound = i
        | otherwise      = go (i + 1) (f z)