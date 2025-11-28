module Main where

import Polynomial 
import Data.Complex

main :: IO ()
main = do
    -- Define P(z) = z^2 + 1
    let p = Poly [1, 0, 1] 
    
    -- Define Q(z) = 2z
    let q = Poly [0, 2]

    -- This calculates P(Q(z)) -> (2z)^2 + 1 -> 4z^2 + 1
    let result = p <> q 
    
    print result
    -- Output: [1.0 :+ 0.0, 0.0 :+ 0.0, 4.0 :+ 0.0]

    -- Chain multiple compositions: P(Q(P(z)))
    let chain = p <> q <> p
    
    print chain
    print $ eval chain 1