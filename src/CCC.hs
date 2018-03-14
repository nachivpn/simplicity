{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies,ScopedTypeVariables #-}
module CCC where

{-
 New features:
 * Pairs in lambda terms
 * Coproducts (case-of)
-}

{-- Objects --}
data T
-- data a :=>: b  -- Exponentials
data a :*: b   -- Products
data a :+: b   -- Coproducts

class Types a where
  size  :: a -> Int
  padl  :: a -> Int
  padr  :: a -> Int

instance Types T where
    size _  = 0
    padl  _ = 0
    padr _  = 0

instance (Types a, Types b) => Types (a :+: b) where
    size _ = let 
                a = size (undefined :: a)
                b = size (undefined :: b) 
            in 1 + max a b
    padl _ = let
                a = size (undefined :: a)
                b = size (undefined :: b) 
            in (max a b) - a
    padr _ = let
                a = size (undefined :: a)
                b = size (undefined :: b) 
            in (max a b) - b
  
instance (Types a, Types b) => Types (a :*: b) where
    size _ = let 
                a = size (undefined :: a)
                b = size (undefined :: b)
            in a + b
    padl _ = padl (undefined :: a)
    padr _ = padr (undefined :: a)

{-- Morphisms --}
data Mph a b where
  -- Id, and composition (present in every category)
    Id       :: Mph a a
    O        :: (Types a, Types b, Types c) => Mph b c -> Mph a b -> Mph a c
    -- When having products
    Factor   :: Mph a b1 -> Mph a b2 -> Mph a (b1 :*: b2)
    -- Pairs proyections
    Fst      :: Mph (a :*: b) a
    Snd      :: Mph (a :*: b) b
    -- Exponentials
    -- Curry    :: Mph (c :*: a) b -> Mph c (a :=>:  b)
    -- Eval     :: Mph ((a :=>:b) :*: a) b
    -- Terminal
    Terminal :: Mph a T
    -- Coproducts
    Inj1CCC :: Mph a (a :+: b)
    Inj2CCC :: Mph b (a :+: b)
    Copair  :: Mph (e :*: a) c -> Mph (e :*: b) c -> Mph (e :*: (a :+: b)) c

-- uncurryC :: (Types b, Types c, Types a) => Mph a (b :=>: c) -> Mph (a :*: b) c
-- uncurryC f = Eval `o` prod f Id

-- CCC to String
toString :: (Mph r a) -> String
toString Id             = "Id"
toString (O g f)        = "(" ++ toString g ++ " o " ++ toString f ++ ")"
toString (Factor f g)   = "<" ++ toString f ++ ", " ++ toString g ++ ">"
toString Fst            = "Fst"
toString Snd            = "Snd"
-- toString (Curry l)      = "(Curry " ++ toString l ++ ")"
-- toString Eval           = "Eval"
toString Terminal       = "Terminal"
toString Inj1CCC        = "Inj1"
toString Inj2CCC        = "Inj2"
toString (Copair f g)   = "[" ++ toString f ++ ", " ++ toString g ++ "]"

{- Cosmetic functions -}
o :: (Types b, Types c, Types a) => Mph b c -> Mph a b -> Mph a c
o = O

x :: Mph a b1 -> Mph a b2 -> Mph a (b1 :*: b2)
x = Factor

-- Factor can write Product very easily. It's needed to encode conditionals
prod :: (Types a1, Types a2, Types b1,Types b2) => Mph a1 b1 -> Mph a2 b2 -> Mph (a1 :*: a2) (b1 :*: b2)
prod f1 f2 = (f1 `o` Fst) `x` (f2 `o` Snd)

-- Products are symmetric wrt isomorphism
prodFlip :: Mph (a :*: e) (e :*: a)
prodFlip = Factor Snd Fst