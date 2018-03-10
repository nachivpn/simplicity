{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, TypeInType #-}
module CCC2SBM where

import CCC
import SBM

-- Approach 1 (Types type class)

class Types a where
    size :: a -> Int
    
instance Types T where
    size _ = 0

instance (Types a, Types b) => Types (a :+: b) where
    size _ = let 
                a = size (undefined :: a)
                b = size (undefined :: b) 
            in 1 + max a b
  
instance (Types a, Types b) => Types (a :*: b) where
    size _ = let 
                a = size (undefined :: a)
                b = size (undefined :: b) 
            in a + b

c2s :: (Types a, Types b) => Mph a b -> [Instr]
c2s (Id :: Mph a b) = [Copy $ size (undefined :: a)]
c2s ((f :: Mph c b) `O` (g :: Mph a c))  =
    [NewFrame $ size (undefined :: b)] 
    -- ++ c2s g ++ [MoveFrame] ++ c2s f ++ [DropFrame]


-- Approach 2 (Type families)

data Nat = Z | S Nat

data SNat (n :: Nat) where
    SZero :: SNat Z
    SSuc  :: SNat n -> SNat (S n) 

toNat :: SNat n -> Int
toNat SZero    = 0
toNat (SSuc n) = 1 + toNat n

type family a :++: b where
    Z :++: b = b
    a :++: Z = a
    (S a) :++: (S b) = S (S (a :++: b))

type family Max a b where
    Max Z b = b
    Max a Z = a
    Max (S a) (S b) = S (Max a b)

type family BitSize a where
    BitSize T = Z
    BitSize (a :+: b) = S (Max (BitSize a) (BitSize b))
    BitSize (a :*: b) = (BitSize a) :++: (BitSize b)

bitSize :: Mph a b -> SNat (BitSize b)
bitSize Terminal = SZero
