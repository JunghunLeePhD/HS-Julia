module Polynomial where

import Data.Complex 
import Data.List (dropWhileEnd) 
import Data.Semigroup
import Data.Monoid

newtype Poly a = Poly { unPoly :: [a] }
    deriving (Show)

instance (Eq a, Num a) => Eq (Poly a) where
    (Poly xs) == (Poly ys) = normalize xs == normalize ys
      where normalize = dropWhileEnd (== 0)

instance Functor Poly where
    fmap f (Poly xs) = Poly (f <$> xs)

instance (Eq a, Num a) => Num (Poly a) where
    (+) (Poly xs) (Poly ys) = Poly $ addLists xs ys
        where
            addLists [] ys = ys
            addLists xs [] = xs
            addLists (x:xs) (y:ys) = (x + y) : addLists xs ys

    (*) (Poly xs) (Poly ys) = Poly $ foldr (\x acc -> (x `scale` ys) `add` (0 : acc)) [] xs
        where
            scale s = map (*s)
            add [] ys = ys
            add xs [] = xs
            add (x:xs) (y:ys) = (x+y) : add xs ys

    negate (Poly xs) = Poly $ negate <$> xs
    
    fromInteger n = Poly [fromInteger n]
    
    abs _ = error "abs not supported for Polynomials"
    signum _ = error "signum not supported for Polynomials"

eval :: Num a => Poly a -> (a -> a)
eval (Poly coeffs) = \z -> foldr (\c acc -> c + z * acc) 0 coeffs

compose :: (Num a, Eq a) => Poly a -> Poly a -> Poly a
compose (Poly p) q = foldr step (Poly [0]) p
  where
    step coeff acc = Poly [coeff] + (q * acc) 

instance (Num a, Eq a) => Semigroup (Poly a) where
    p <> q = compose p q

instance (Num a, Eq a) => Monoid (Poly a) where
    mempty = Poly [0, 1] 