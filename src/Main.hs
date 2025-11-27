module Main where

import Polynomial
import Data.Complex

main :: IO ()
main = do
    -- Case 1: Exact Zero
    let p1 = [1:+0, 2:+0, 0:+0]
    putStrLn "Normalizing exact zero:"
    print (normalize p1) 
    -- Output: [1.0 :+ 0.0, 2.0 :+ 0.0]

    -- Case 2: Computational Zero (Very small number)
    let p2 = [1:+0, 2:+0, 0.00000000000001 :+ 0] 
    putStrLn "Normalizing computational zero:"
    print (normalize p2)
    -- Output: [1.0 :+ 0.0, 2.0 :+ 0.0]
    
    -- Case 3: Zero Polynomial
    let p3 = [0:+0, 0:+0]
    putStrLn "Normalizing zero polynomial:"
    print (normalize p3)
    -- Output: []