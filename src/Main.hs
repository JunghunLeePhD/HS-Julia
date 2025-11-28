module Main where

import Ring
import Data.Complex

main :: IO ()
main = do
    putStrLn "====== Ring & Module Test ======"

    -- 1. Test Ring Int
    putStrLn "\n--- 1. Ring Int Test ---"
    let a = 10 :: Int
    let b = 3 :: Int
    putStrLn $ "a = " ++ show a ++ ", b = " ++ show b
    putStrLn $ "Addition (a <+> b):       " ++ show (a <+> b)
    putStrLn $ "Subtraction (a <-> b):    " ++ show (a <-> b)
    putStrLn $ "Multiplication (a <.> b): " ++ show (a <.> b)

    -- 2. Test Module Double over Double
    -- (A simple scalar field is a module over itself)
    putStrLn "\n--- 2. Module Double (over Double) ---"
    let s = 2.5 :: Double    -- Scalar
    let v = 4.0 :: Double    -- Vector (which is just a number here)
    putStrLn $ "Scalar s = " ++ show s ++ ", Vector v = " ++ show v
    putStrLn $ "Scalar Mult (s ^.^ v):     " ++ show (s ^.^ v)

    -- 3. Test Module (Complex Double) over Double
    -- This is the classic 2D vector space test
    putStrLn "\n--- 3. Module Complex Double (over Double) ---"
    let scalar = 2.0 :: Double
    let v1 = 1.0 :+ 2.0 :: Complex Double -- 1 + 2i
    let v2 = 3.0 :+ 4.0 :: Complex Double -- 3 + 4i

    putStrLn $ "Scalar r = " ++ show scalar
    putStrLn $ "Vector v1 = " ++ show v1
    putStrLn $ "Vector v2 = " ++ show v2

    -- Calculation: r * v1 + v2
    -- = 2.0 * (1 + 2i) + (3 + 4i)
    -- = (2 + 4i) + (3 + 4i)
    -- = 5 + 8i
    let result = (scalar ^.^ v1) ^+^ v2
    
    putStrLn $ "Calculation (r ^.^ v1 ^+^ v2): " ++ show result
    
    -- Verify Subtraction
    let subResult = v2 ^-^ v1
    putStrLn $ "Subtraction (v2 ^-^ v1):      " ++ show subResult