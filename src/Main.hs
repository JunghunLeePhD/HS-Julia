module Main where

import Data.Complex
import Polynomial
import Orbit

-- | Simple assertion helper
assert :: String -> Bool -> IO ()
assert name condition = do
    if condition
        then putStrLn $ "[PASS] " ++ name
        else putStrLn $ "[FAIL] " ++ name

main :: IO ()
main = do
    putStrLn "--- Test: P(z) = z^2 + 1 ---"
    
    -- Define Polynomial: z^2 + 1 
    -- Coeffs are [1, 0, 1] for 1 + 0z + 1z^2
    let p = Poly [1 :+ 0, 0 :+ 0, (-1) :+ 0] 
    
    let startZ = 0 :+ 1 
    let iterations = 5
    
    -- Run the function
    let result = orbit p iterations startZ :: [Complex Double]

    -- Print results nicely
    mapM_ print result

    where
        printStep (n, z) = putStrLn $ "Iter " ++ show n ++ ": " ++ show z