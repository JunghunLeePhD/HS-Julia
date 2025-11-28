{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE UndecidableInstances #-}

module Ring where

import Data.Complex

class (Eq a) => Ring a where
    add  :: a -> a -> a
    zero :: a
    inv  :: a -> a
    mul  :: a -> a -> a
    one  :: a

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

instance Ring Int where
    add = (+)
    zero = 0
    inv = negate
    mul = (*)
    one = 1

instance Ring Float where
    add = (+)
    zero = 0
    inv = negate
    mul = (*)
    one = 1

instance Ring Double where
    add = (+)
    zero = 0
    inv = negate
    mul = (*)
    one = 1

instance (Ring a) => Ring (Complex a) where
    add (x1 :+ y1) (x2 :+ y2) = (x1 `add` x2) :+ (y1 `add` y2)
    zero = zero :+ zero
    inv (x :+ y) = (inv x) :+ (inv y)
    
    mul (x1 :+ y1) (x2 :+ y2) = 
        (x1 `mul` x2 `sub` y1 `mul` y2) :+ (x1 `mul` y2 `add` y1 `mul` x2)
    
    one = one :+ zero

class (Ring r, Eq m) => Module r m | m -> r where
    vadd  :: m -> m -> m
    vzero :: m
    vinv  :: m -> m
    smul  :: r -> m -> m

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

instance Module Int Int where
    vadd = add
    vzero = zero
    vinv = inv
    smul = mul

instance Module Float Float where
    vadd = add
    vzero = zero
    vinv = inv
    smul = mul

instance Module Double Double where
    vadd = add
    vzero = zero
    vinv = inv
    smul = mul

instance (Ring a) => Module a (Complex a) where
    vadd = add
    vzero = zero
    vinv = inv
    smul r (x :+ y) = (r `mul` x) :+ (r `mul` y)