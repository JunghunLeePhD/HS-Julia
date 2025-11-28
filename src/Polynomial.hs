{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Polynomial where

import Ring
import Data.List (dropWhileEnd)

newtype Poly a = Poly { unPoly :: [a] }
    deriving (Show)

instance Functor Poly where
    fmap f (Poly xs) = Poly (f <$> xs)

normalize :: (Ring a) => [a] -> [a]
normalize = dropWhileEnd (== zero)

instance (Ring a) => Eq (Poly a) where
    (Poly xs) == (Poly ys) = normalize xs == normalize ys

instance (Ring a) => Ring (Poly a) where
    zero = Poly []
    one  = Poly [one]
    
    add (Poly xs) (Poly ys) = Poly $ normalize $ addLists xs ys
      where
        addLists [] b = b
        addLists a [] = a
        addLists (a:as) (b:bs) = (a <+> b) : addLists as bs

    inv (Poly xs) = Poly $ inv <$> xs

    mul (Poly xs) (Poly ys) = Poly $ normalize $ go xs ys
      where
        go _ [] = []
        go [] _ = []
        go (a:as) bs = addLists ((mul a) <$> bs) (zero : go as bs)
        
        addLists [] b = b
        addLists a [] = a
        addLists (x:xs') (y:ys') = (x <+> y) : addLists xs' ys'

instance (Ring a) => Module a (Poly a) where
    vadd = add
    vzero = zero
    vinv = inv
    
    smul c (Poly xs) = Poly $ normalize $ (mul c) <$> xs

eval :: (Ring a) => Poly a -> (a -> a)
eval (Poly coeffs) = \z -> foldr (\c acc -> c <+> (z <.> acc)) zero coeffs

compose :: (Ring a) => Poly a -> Poly a -> Poly a
compose (Poly coeffs) q = foldr step zero coeffs
  where
    step c acc = (Poly [c]) <+> (q <.> acc)

instance (Ring a) => Semigroup (Poly a) where
    (<>) = compose

instance (Ring a) => Monoid (Poly a) where
    mempty = Poly [zero, one] 