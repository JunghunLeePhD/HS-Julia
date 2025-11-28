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
    putStrLn "======================================="
    putStrLn "         Orbit Logic Test Suite        "
    putStrLn "======================================="

    -- Setup Common Polynomials
    -- Standard Monic Polynomial z^2
    let pStandard = mkPoly [0, 0, 1] :: Poly (Complex Double)
    -- Polynomial 2z^2 + 10 (Large offset, large leading coeff)
    let pLarge = mkPoly [10, 0, 2] :: Poly (Complex Double)

    ----------------------------------------------------------------
    -- 1. TESTING ORBIT GENERATION (Trace)
    ----------------------------------------------------------------
    putStrLn "\n--- Testing orbit (trace) ---"
    
    -- Using z^2. Orbit of 2 -> 4 -> 16 -> 256
    let startVal = 2.0 :+ 0.0
    let trace = orbit pStandard 4 startVal
    
    putStrLn $ "  Trace (z^2, start=2): " ++ show trace
    assert "Orbit length is correct" (length trace == 4)
    assert "Orbit values are correct" (take 3 trace == [2 :+ 0, 4 :+ 0, 16 :+ 0])


    ----------------------------------------------------------------
    -- 2. TESTING ESCAPE TIME (Logic Check)
    ----------------------------------------------------------------
    putStrLn "\n--- Testing escapeTime ---"

    -- For 2z^2 + 10, the dynamic radius calculation (handled internally)
    -- should determine a safe bound (approx 6.0).
    -- If we start at z=0:
    -- Step 0: z=0. 
    -- Step 1: z=10. |10| > 6.0. Returns 1.
    let steps = escapeTime pLarge 10 (0 :+ 0)
    
    putStrLn $ "  Steps to escape for 2z^2+10 starting at 0: " ++ show steps
    assert "Escape logic works (escapes immediately)" (steps == 1)

    -- For z^2, radius is 2.0.
    -- Start at 0.5. 0.5 -> 0.25 -> ... (stays small).
    let stepsBounded = escapeTime pStandard 50 (0.5 :+ 0)
    assert "Bounded point hits maxIter" (stepsBounded == 50)


    ----------------------------------------------------------------
    -- 3. TESTING ORBIT STATUS (Tuple Return)
    ----------------------------------------------------------------
    putStrLn "\n--- Testing orbitWithCount ---"

    -- Case: Escaped
    -- z^2 starting at 3. Radius is 2. |3| > 2 immediately.
    let (valEsc, countEsc) = orbitWithCount pStandard 10 (3 :+ 0)
    assert "orbitWithCount (Escaped) returns Nothing" (valEsc == Nothing)
    assert "orbitWithCount (Escaped) count is 0" (countEsc == 0)

    -- Case: Bounded
    -- z^2 starting at 0.
    let (valBnd, countBnd) = orbitWithCount pStandard 5 (0 :+ 0)
    assert "orbitWithCount (Bounded) returns Just" (valBnd == Just 0)
    assert "orbitWithCount (Bounded) count is maxIter" (countBnd == 5)

    putStrLn "\n======================================="
    putStrLn "           Tests Completed             "
    putStrLn "======================================="
