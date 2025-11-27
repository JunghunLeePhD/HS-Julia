module Main where

import Polynomial
import Data.Complex

main :: IO ()
main = do
    -- P(z) = 1 + 1z
    let p = [1:+0, 1:+0]
    
    -- Q(z) = 0 + 0z + 1z^2
    let q = [0:+0, 0:+0, 1:+0]
    
    putStrLn "P(z):"
    print p
    
    putStrLn "Q(z):"
    print q
    
    putStrLn "Composition P(Q(z)) -> Should be 1 + z^2:"
    let result = compose p q
    print result 
    -- Expected output: [1.0 :+ 0.0, 0.0 :+ 0.0, 1.0 :+ 0.0]