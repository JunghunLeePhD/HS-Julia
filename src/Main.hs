module Main where

import Data.Complex
import Polynomial
import Orbit
import Ring 

assert :: String -> Bool -> IO ()
assert name condition = do
    putStr $ "Testing " ++ name ++ "... "
    if condition
        then putStrLn "PASSED"
        else putStrLn "FAILED"

-- Approximate equality for floating point comparisons
approxEq :: Double -> Double -> Bool
approxEq a b = abs (a - b) < 1e-9

main :: IO ()
main = do
    putStrLn "=== Running NormedRing Tests ==="

    -- 1. Test Int Instance
    -- Expected: norm (-5) should be 5.0
    let i = -5 :: Int
    assert "Int (negative)" $ norm i == 5.0
    assert "Int (zero)"     $ norm (0 :: Int) == 0.0

    -- 2. Test Float Instance
    -- Expected: norm (-3.5) should be 3.5
    let f = -3.5 :: Float
    assert "Float" $ norm f `approxEq` 3.5

    -- 3. Test Double Instance
    -- Expected: norm (-10.123) should be 10.123
    let d = -10.123 :: Double
    assert "Double" $ norm d == 10.123

    -- 4. Test Complex Double Instance
    -- Expected: norm (3 + 4i) should be 5.0 (Pythagorean triple)
    let cDouble = 3 :+ 4 :: Complex Double
    assert "Complex Double (3+4i)" $ norm cDouble == 5.0

    -- 5. Test Complex Float Instance
    -- Expected: norm (1 + 1i) should be sqrt(2) approx 1.41421356
    let cFloat = 1 :+ 1 :: Complex Float
    assert "Complex Float (1+1i)" $ norm cFloat `approxEq` sqrt 2

    putStrLn "=== All Tests Completed ==="