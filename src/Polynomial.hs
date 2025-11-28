{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Polynomial where

import Data.List (dropWhileEnd)
import Data.Monoid (Endo(..))
import Ring

newtype Poly a = Poly { unPoly :: [a] }
    deriving (Show)

normalize :: (Ring a) => [a] -> [a]
normalize = dropWhileEnd (== zero)

mkPoly :: (Ring a) => [a] -> Poly a
mkPoly xs = Poly $ normalize xs

mapPoly :: (Ring a, Ring b) => (a -> b) -> Poly a -> Poly b
mapPoly f (Poly xs) = mkPoly (map f xs)

instance (Ring a) => Eq (Poly a) where
    (Poly xs) == (Poly ys) = normalize xs == normalize ys

instance (Ring a) => Ring (Poly a) where
    zero = Poly [zero]
    one  = Poly [one]
    inv (Poly xs) = Poly (inv <$> xs)

    add (Poly xs) (Poly ys) = mkPoly (addLists xs ys)
      where
        addLists [] b = b
        addLists a [] = a
        addLists (a:as) (b:bs) = (a <+> b) : addLists as bs

    mul (Poly xs) (Poly ys) = mkPoly (go xs ys)
      where
        go _ [] = []
        go [] _ = []
        go (a:as) bs = 
            let 
                term1 = map (a <.>) bs
                term2 = zero : go as bs
            in 
                addLists term1 term2
        
        addLists [] b = b
        addLists a [] = a
        addLists (x:xs') (y:ys') = (x <+> y) : addLists xs' ys'

composeSymbolic :: (Ring a) => Poly a -> Poly a -> Poly a
composeSymbolic (Poly coeffs) q = foldr step zero coeffs
  where
    step c acc = (mkPoly [c]) <+> (q <.> acc)

toEndo :: (Ring a) => Poly a -> Endo a
toEndo (Poly coeffs) = Endo (\x -> hornersMethod coeffs x)
  where
    hornersMethod [] _ = zero
    hornersMethod [c] _ = c
    hornersMethod (c:cs) x = c <+> (x <.> hornersMethod cs x)
