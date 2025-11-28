module Main where

import Ring
import Polynomial
import Data.Complex

main :: IO ()
main = do
    putStrLn "--- Fixed Poly Implementation ---"
    
    -- P = 1 + 2x
    let p = mkPoly [1, 2] :: Poly Int
    -- Q = 0 + 3x (Testing normalization of leading zeros)
    let q = mkPoly [0, 3, 0, 0] :: Poly Int
    
    putStrLn $ "P: " ++ show p
    putStrLn $ "Q (normalized): " ++ show q
    
    putStrLn "\n--- Arithmetic ---"
    putStrLn $ "P + Q: " ++ show (p <+> q)
    putStrLn $ "P * Q: " ++ show (p <.> q)
    
    putStrLn "\n--- The Map Fix ---"
    -- This would be unsafe with Functor fmap:
    -- Mapping everything to zero must result in an empty list Poly, not [0,0]
    let zeroed = mapPoly (\_ -> 0) p :: Poly Int
    putStrLn $ "Mapped to zero: " ++ show zeroed
    putStrLn $ "Is it equal to zero? " ++ show (zeroed == zero)