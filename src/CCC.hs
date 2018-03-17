{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module CCC where

{-- Objects --}
data T
data a :*: b   -- Products
data a :+: b   -- Coproducts

{-- Morphisms --}
data Mph a b where
  -- Id, and composition (present in every category)
    Id       :: Mph a a
    O        :: Mph b c -> Mph a b -> Mph a c
    -- When having products
    Factor   ::  Mph a b1 -> Mph a b2 -> Mph a (b1 :*: b2)
    -- Pairs proyections
    Fst      :: Mph (a :*: b) a
    Snd      :: Mph (a :*: b) b
    -- Terminal
    Terminal :: Mph a T
    -- Coproducts
    Inj1CCC :: Mph a (a :+: b)
    Inj2CCC :: Mph b (a :+: b)
    Copair  :: Mph (e :*: a) c -> Mph (e :*: b) c -> Mph (e :*: (a :+: b)) c

-- CCC to String
toString :: (Mph r a) -> String
toString Id             = "Id"
toString (O g f)        = "(" ++ toString g ++ " o " ++ toString f ++ ")"
toString (Factor f g)   = "<" ++ toString f ++ ", " ++ toString g ++ ">"
toString Fst            = "Fst"
toString Snd            = "Snd"
toString Terminal       = "Terminal"
toString Inj1CCC        = "Inj1"
toString Inj2CCC        = "Inj2"
toString (Copair f g)   = "[" ++ toString f ++ ", " ++ toString g ++ "]"

{- Cosmetic functions -}
o :: Mph b c -> Mph a b -> Mph a c
o = O

x :: Mph a b1 -> Mph a b2 -> Mph a (b1 :*: b2)
x = Factor

-- Factor can write Product very easily. It's needed to encode conditionals
prod :: Mph a1 b1 -> Mph a2 b2 -> Mph (a1 :*: a2) (b1 :*: b2)
prod f1 f2 = (f1 `o` Fst) `x` (f2 `o` Snd)

-- Products are symmetric wrt isomorphism
prodFlip :: Mph (a :*: e) (e :*: a)
prodFlip = Factor Snd Fst


