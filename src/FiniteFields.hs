{-#language MultiParamTypeClasses #-}
module FiniteFields where

import Numeric.Additive.Class
import Numeric.Algebra
import Numeric.Rig.Class
import Numeric.Semiring.ZeroProduct
import Numeric.Domain.Class
import Numeric.Domain.Integral
import Numeric.Algebra.Unital.UnitNormalForm
import Numeric.Domain.GCD
import Numeric.Domain.UFD
import Numeric.Domain.PID
import Numeric.Domain.Euclidean
import Numeric.Decidable.Units
import Numeric.Decidable.Zero
import Numeric.Decidable.Associates

import Debug.Trace

data Z2 = Z2Zero | Z2Unit deriving Eq

instance Show Z2 where
   show Z2Zero = "0"
   show _      = "1"

instance Additive Z2 where
   Z2Zero + a = a
   a + Z2Zero = a
   _ + _      = Z2Zero

instance LeftModule Natural Z2 where
    nat .* a = (toInteger nat) .* a

instance RightModule Natural Z2 where
    a *. nat = nat .* a

instance LeftModule Integer Z2 where
    int .* Z2Zero = Z2Zero
    int .* _ | int `mod` 2 == 1 = Z2Unit
             | otherwise        = Z2Zero

instance RightModule Integer Z2 where
    a *. int = int .* a

instance Monoidal Z2 where
    zero = Z2Zero

instance Abelian Z2 

instance Multiplicative Z2 where
    Z2Zero * _ = Z2Zero
    _ * Z2Zero = Z2Zero
    _ * _      = Z2Unit

instance Semiring Z2
instance ZeroProductSemiring Z2

instance Unital Z2 where
    one = Z2Unit

instance DecidableUnits Z2 where
    recipUnit Z2Unit = Just Z2Unit
    recipUnit _      = Nothing

instance DecidableAssociates Z2 where
    isAssociate a b = a == b

instance DecidableZero Z2 where
    isZero Z2Zero = True
    isZero _      = False

instance Rig Z2 

instance Group Z2 where
    negate =  id 

instance Ring Z2 
--instance Domain Z2 -- Why exactly that??????????
instance Commutative Z2
instance IntegralDomain Z2
instance UnitNormalForm Z2 
instance GCDDomain Z2
instance UFD Z2
instance PID Z2
instance Euclidean Z2
instance Division Z2 where
    recip Z2Zero = error "Zero divison in Z2!" 
    recip Z2Unit = Z2Unit
--instance Field Z2




