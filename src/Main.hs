module Main where

import Ring
import Polynomial
import Orbit
import Data.Complex
import Text.Printf (printf)

-- | Test assertion helper
assert :: String -> Bool -> IO ()
assert name condition = do
    putStr $ "Testing " ++ name ++ "... "
    if condition
        then putStrLn "PASSED"
        else putStrLn $ "FAILED"

-- | Floating point equality with tolerance
approxEq :: Double -> Double -> Bool
approxEq a b = abs (a - b) < 1e-9

main :: IO ()
main = do
    putStrLn "=== Running Orbit Module Tests ==="

    -- TEST 1: Simple Monomial z^2
    -- P(z) = 0 + 0z + 1z^2
    -- Bound should be max(2, 1 + max(0)) = 2.0
    let p1 = mkPoly [0, 0, 1] :: Poly (Complex Double)
    let bound1 = escapeRadius p1
    assert "Escape Radius (z^2)" $ bound1 == 2.0

    -- TEST 2: Polynomial with Larger Bound
    -- P(z) = 2z^2 - 8
    -- Coefficients: [-8, 0, 2]
    -- Norms: [8, 0, 2]
    -- Leading term magnitude: 2
    -- Ratios: |0|/2 = 0, |-8|/2 = 4
    -- Cauchy Bound formula: 1 + max(ratios) = 1 + 4 = 5.0
    let p2 = mkPoly [-8, 0, 2] :: Poly (Complex Double)
    let bound2 = escapeRadius p2
    assert "Escape Radius (2z^2 - 8)" $ bound2 == 5.0

    -- TEST 3: Constant Polynomial (Degenerate case)
    -- P(z) = 5
    -- Should default to 2.0 bound
    let p3 = mkPoly [5] :: Poly (Complex Double)
    assert "Escape Radius (Constant 5)" $ escapeRadius p3 == 2.0

    -- TEST 4: Escape Time - Immediate Escape
    -- Using P(z) = z^2 (Bound = 2.0)
    -- Point z = 3.0 (Outside bound)
    -- Should return 0 iterations
    let zOut = 3.0 :+ 0.0
    let timeOut = escapeTime p1 100 zOut
    assert "Escape Time (Immediate Escape)" $ timeOut == 0

    -- TEST 5: Escape Time - Trapped Point
    -- Using P(z) = z^2
    -- Point z = 0.5 (Inside bound, converges to 0)
    -- Should reach maxIter (100)
    let zIn = 0.5 :+ 0.0
    let timeIn = escapeTime p1 100 zIn
    assert "Escape Time (Trapped Point)" $ timeIn == 100

    -- TEST 6: Escape Time - Slow Escape
    -- Using P(z) = z^2 + 0.5
    -- Start at z = 0.5
    -- Iterations:
    -- 0: 0.5 (norm 0.5)
    -- 1: 0.5^2 + 0.5 = 0.75
    -- 2: 0.75^2 + 0.5 = 1.0625
    -- 3: 1.0625^2 + 0.5 ≈ 1.62
    -- 4: 1.62^2 + 0.5 ≈ 3.15 (Escaped > 2.0)
    -- Expect roughly 4 iterations
    let p4 = mkPoly [0.5, 0, 1] :: Poly (Complex Double)
    let zStart = 0.5 :+ 0.0
    let timeSlow = escapeTime p4 100 zStart
    
    -- Note: escapeRadius for z^2 + 0.5 is 2.0
    -- |0.5|/1 = 0.5. Bound = 1 + 0.5 = 1.5? 
    -- Wait, formula is max(2.0, bound).
    -- Bound calc: 1 + 0.5 = 1.5. Max(2.0, 1.5) = 2.0.
    -- So escape condition is norm > 2.0.
    assert "Escape Time (Slow Escape)" $ timeSlow == 4

    putStrLn "=== All Tests Completed ==="