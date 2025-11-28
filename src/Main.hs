module Main where

import Data.Complex
import Ring -- Imports your module

-- | A helper function to verify Ring Axioms for any given type 'a'.
-- We pass three arbitrary values (x, y, z) to test properties like associativity.
verifyRingLaws :: (Ring a, Eq a, Show a) => String -> a -> a -> a -> IO ()
verifyRingLaws typeName x y z = do
    putStrLn $ "=== Testing Ring Axioms for: " ++ typeName ++ " ==="
    
    -- 1. Addition Associativity: (x + y) + z = x + (y + z)
    check "Add Associativity" $ 
        (x <+> y) <+> z == x <+> (y <+> z)

    -- 2. Addition Commutativity: x + y = y + x
    check "Add Commutativity" $ 
        x <+> y == y <+> x

    -- 3. Additive Identity: x + 0 = x
    check "Add Identity (Zero)" $ 
        x <+> zero == x

    -- 4. Additive Inverse: x + (-x) = 0
    check "Additive Inverse" $ 
        x <+> (inv x) == zero

    -- 5. Multiplication Associativity: (x * y) * z = x * (y * z)
    check "Mul Associativity" $ 
        (x <.> y) <.> z == x <.> (y <.> z)

    -- 6. Multiplicative Identity: x * 1 = x
    check "Mul Identity (One)" $ 
        x <.> one == x

    -- 7. Distributivity: x * (y + z) = (x * y) + (x * z)
    check "Distributivity" $ 
        x <.> (y <+> z) == (x <.> y) <+> (x <.> z)
        
    putStrLn ""

-- Helper to print Pass/Fail
check :: String -> Bool -> IO ()
check name True  = putStrLn $ "  [PASS] " ++ name
check name False = putStrLn $ "  [FAIL] " ++ name

main :: IO ()
main = do
    ------------------------------------------------------------
    -- Test 1: Standard Integers
    ------------------------------------------------------------
    verifyRingLaws "Int" (5 :: Int) 3 10

    ------------------------------------------------------------
    -- Test 2: Complex Doubles
    -- This tests standard floating point complex numbers
    ------------------------------------------------------------
    let c1 = 1 :+ 2 :: Complex Double
    let c2 = 3 :+ 4 :: Complex Double
    let c3 = 0.5 :+ 0.5 :: Complex Double
    verifyRingLaws "Complex Double" c1 c2 c3

    ------------------------------------------------------------
    -- Test 3: Complex Integers (Gaussian Integers)
    -- This proves your recursive 'instance Ring a => Ring (Complex a)' works!
    ------------------------------------------------------------
    let g1 = 1 :+ 1 :: Complex Int
    let g2 = 2 :+ 0 :: Complex Int
    let g3 = 0 :+ (-1) :: Complex Int
    verifyRingLaws "Complex Int (Gaussian)" g1 g2 g3

    ------------------------------------------------------------
    -- Manual Calculation Check
    -- (1 + i) * (1 - i) should be 2
    ------------------------------------------------------------
    putStrLn "=== Manual Calculation Check ==="
    let z1 = 1 :+ 1 :: Complex Int
    let z2 = 1 :+ (-1) :: Complex Int
    let result = z1 <.> z2
    
    putStrLn $ "Calculation: (1+i) * (1-i)"
    putStrLn $ "Expected: 2 :+ 0"
    putStrLn $ "Actual:   " ++ show result
    if result == (2 :+ 0) then putStrLn "  [PASS]" else putStrLn "  [FAIL]"