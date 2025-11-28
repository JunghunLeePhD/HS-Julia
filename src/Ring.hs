{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-} 
{-# LANGUAGE TypeFamilies #-}      

module Ring where

import Data.Complex

class (Eq a) => Ring a where
    add  :: a -> a -> a
    -- Default implementation for types that are instances of Num
    default add :: Num a => a -> a -> a
    add = (+)

    zero :: a
    default zero :: Num a => a
    zero = 0

    inv  :: a -> a
    default inv :: Num a => a -> a
    inv = negate

    mul  :: a -> a -> a
    default mul :: Num a => a -> a -> a
    mul = (*)

    one  :: a
    default one :: Num a => a
    one = 1

    sub :: a -> a -> a
    sub x y = add x (inv y)

infixl 6 <+>, <->
infixl 7 <.>

(<+>) :: (Ring a) => a -> a -> a
(<+>) = add

(<->) :: (Ring a) => a -> a -> a
(<->) = sub

(<.>) :: (Ring a) => a -> a -> a
(<.>) = mul

-- Simplified Ring Instances
-- Since Int, Float, and Double are instances of Num, 
-- they automatically use the default implementations.
instance Ring Int
instance Ring Float
instance Ring Double

instance (Ring a) => Ring (Complex a) where
    add (x1 :+ y1) (x2 :+ y2) = (x1 `add` x2) :+ (y1 `add` y2)
    zero = zero :+ zero
    inv (x :+ y) = (inv x) :+ (inv y)
    
    mul (x1 :+ y1) (x2 :+ y2) = 
        (x1 `mul` x2 `sub` y1 `mul` y2) :+ (x1 `mul` y2 `add` y1 `mul` x2)
    
    one = one :+ zero

class (Ring r, Eq m) => Module r m | m -> r where
    vadd  :: m -> m -> m
    -- Define default implementations that work when r == m
    default vadd :: (r ~ m) => m -> m -> m
    vadd = add

    vzero :: m
    default vzero :: (r ~ m) => m
    vzero = zero

    vinv  :: m -> m
    default vinv :: (r ~ m) => m -> m
    vinv = inv

    smul  :: r -> m -> m
    default smul :: (r ~ m) => r -> m -> m
    smul = mul

    vsub  :: m -> m -> m
    vsub x y = vadd x (vinv y)

infixr 7 ^.^
infixl 6 ^+^
infixl 6 ^-^

(^.^) :: (Module r m) => r -> m -> m
(^.^) = smul

(^+^) :: (Module r m) => m -> m -> m
(^+^) = vadd

(^-^) :: (Module r m) => m -> m -> m
(^-^) = vsub

-- Instances
-- Since Int ~ Int, it uses the default (Ring) implementation automatically.
instance Module Int Int
instance Module Float Float
instance Module Double Double

-- Complex Instance (Must remain explicit because r != m)
instance (Ring a) => Module a (Complex a) where
    vadd = add
    vzero = zero
    vinv = inv
    smul r (x :+ y) = (r `mul` x) :+ (r `mul` y)