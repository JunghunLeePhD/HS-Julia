module Main where

import Ring
import Polynomial
import Data.Complex

main :: IO ()
main = do
    let logOp desc val = putStrLn $ desc ++ ":\t" ++ show val
    
    putStrLn "====== Ring, Module & Polynomial Test ======"

    -- 1. Define some Polynomials
    -- p1 = 1 + 2x
    let p1 = Poly [1.0, 2.0] :: Poly Double
    -- p2 = 3 + 4x
    let p2 = Poly [3.0, 4.0] :: Poly Double

    putStrLn "\n--- Definitions ---"
    logOp "p1 (1 + 2x)" p1
    logOp "p2 (3 + 4x)" p2

    -- 2. Ring Operations
    putStrLn "\n--- Ring Operations ---"
    -- Addition: (1+3) + (2+4)x = 4 + 6x
    logOp "Add (p1 <+> p2)" (p1 <+> p2)
    
    -- Multiplication: (1+2x)(3+4x) = 3 + 4x + 6x + 8x^2 = 3 + 10x + 8x^2
    logOp "Mul (p1 <.> p2)" (p1 <.> p2)

    -- Negation / Inverse
    logOp "Inv (inv p1)"    (inv p1)
    
    -- Subtraction: p2 - p1 = (3-1) + (4-2)x = 2 + 2x
    logOp "Sub (p2 <-> p1)" (p2 <-> p1)

    -- 3. Module Operations
    putStrLn "\n--- Module Operations ---"
    -- Scalar Mult: 10 * (1+2x) = 10 + 20x
    let scalar = 10.0 :: Double
    logOp "Scalar (10 ^.^ p1)" (scalar ^.^ p1)

    -- 4. Evaluation
    putStrLn "\n--- Evaluation ---"
    -- p1(5) = 1 + 2(5) = 11
    putStrLn $ "Eval p1 at 5.0:\t" ++ show (eval p1 5.0)

    -- 5. Composition (Monoid)
    putStrLn "\n--- Composition ---"
    -- p1(p2(x)) = 1 + 2(3+4x) = 1 + 6 + 8x = 7 + 8x
    logOp "Compose (p1 <> p2)" (p1 <> p2)
    
    -- Identity Check: p1(x) should equal p1
    -- mempty is 'x' (Poly [0, 1])
    logOp "Identity (p1 <> mempty)" (p1 <> mempty)

    -- 6. Normalization Check
    putStrLn "\n--- Normalization ---"
    let pUnnormalized = Poly [1.0, 2.0, 0.0, 0.0] :: Poly Double
    logOp "Unnormalized input" pUnnormalized
    putStrLn $ "Equal to p1?\t" ++ show (pUnnormalized == p1)