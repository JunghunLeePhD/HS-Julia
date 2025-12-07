module Ring where

import Data.Complex

class (Eq a) => Ring a where
    add :: a -> a -> a
    default add :: Num a => a -> a -> a
    add = (+)
    {-# INLINE add #-}

    zero :: a
    default zero :: Num a => a
    zero = 0
    {-# INLINE zero #-}

    neg :: a -> a
    default neg :: Num a => a -> a
    neg = negate
    {-# INLINE neg #-}

    mul :: a -> a -> a
    default mul :: Num a => a -> a -> a
    mul = (*)
    {-# INLINE mul #-}

    one :: a
    default one :: Num a => a
    one = 1
    {-# INLINE one #-}

    sub :: a -> a -> a
    sub x y = add x (neg y)
    {-# INLINE sub #-}

infixl 6 <+>, <->
infixl 7 <.>

(<+>) :: (Ring a) => a -> a -> a
(<+>) = add
{-# INLINE (<+>) #-}

(<->) :: (Ring a) => a -> a -> a
(<->) = sub
{-# INLINE (<->) #-}

(<.>) :: (Ring a) => a -> a -> a
(<.>) = mul
{-# INLINE (<.>) #-}

instance Ring Int
instance Ring Integer 
instance Ring Float
instance Ring Double

instance (Ring a) => Ring (Complex a) where
    add (x1 :+ y1) (x2 :+ y2) = (x1 <+> x2) :+ (y1 <+> y2)
    zero = zero :+ zero
    neg (x :+ y) = (neg x) :+ (neg y)
    
    mul (x1 :+ y1) (x2 :+ y2) = 
        (x1 <.> x2 <-> y1 <.> y2) :+ (x1 <.> y2 <+> y1 <.> x2)
    
    one = one :+ zero
    
    {-# INLINE add #-}
    {-# INLINE neg #-}
    {-# INLINE mul #-}

class (Ring a) => NormedRing a where
    norm :: a -> Double

instance NormedRing Int where
    norm = fromIntegral . abs
    {-# INLINE norm #-}

instance NormedRing Integer where
    norm = fromIntegral . abs
    {-# INLINE norm #-}

instance NormedRing Float where
    norm = realToFrac . abs
    {-# INLINE norm #-}

instance NormedRing Double where
    norm = abs
    {-# INLINE norm #-}

instance (Ring a, RealFloat a) => NormedRing (Complex a) where
    norm = realToFrac . magnitude
    {-# INLINE norm #-}


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
    vinv = neg

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
    vinv = neg
    smul r (x :+ y) = (r `mul` x) :+ (r `mul` y)