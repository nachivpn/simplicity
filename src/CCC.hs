{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies,ScopedTypeVariables #-}
module CCC where

{-- Objects --}
data T
data a :*: b   -- Products
data a :+: b   -- Coproducts

-- |Types is a type class which is implemnted only by Simplicity types
class Types a where
    bsize :: a -> Int
    padl  :: a -> Int
    padr  :: a -> Int
    bsizf :: a -> Int
    bsizs :: a -> Int
  
{-- Morphisms --}
data Mph a b where
  -- Id, and composition (present in every category)
    Id       :: Mph a a
    O        :: (Types a, Types b, Types c) => Mph b c -> Mph a b -> Mph a c
    -- When having products
    Factor   :: (Types a, Types b1, Types b2) => Mph a b1 -> Mph a b2 -> Mph a (b1 :*: b2)
    -- Pairs proyections
    Fst      :: Mph (a :*: b) a
    Snd      :: Mph (a :*: b) b
    -- Terminal
    Terminal :: Mph a T
    -- Coproducts
    Inj1CCC :: Mph a (a :+: b)
    Inj2CCC :: Mph b (a :+: b)
    Copair  :: (Types e, Types a, Types b, Types c) => 
        Mph (e :*: a) c -> Mph (e :*: b) c -> Mph (e :*: (a :+: b)) c

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
o :: (Types b, Types c, Types a) => Mph b c -> Mph a b -> Mph a c
o = O

x :: (Types a, Types b1, Types b2) => Mph a b1 -> Mph a b2 -> Mph a (b1 :*: b2)
x = Factor

-- Factor can write Product very easily. It's needed to encode conditionals
prod :: (Types a1, Types a2, Types b1,Types b2) => Mph a1 b1 -> Mph a2 b2 -> Mph (a1 :*: a2) (b1 :*: b2)
prod f1 f2 = (f1 `o` Fst) `x` (f2 `o` Snd)

-- Products are symmetric wrt isomorphism
prodFlip :: (Types a, Types e) => Mph (a :*: e) (e :*: a)
prodFlip = Factor Snd Fst

instance Types T where
    bsize _ = 0
    padl  _ = undefined
    padr _  = undefined
    bsizf _ = undefined
    bsizs _ = undefined

instance (Types a, Types b) => Types (a :+: b) where
    bsize _ = let 
                a = bsize (undefined :: a)
                b = bsize (undefined :: b) 
            in 1 + max a b
    padl _ = let
                a = bsize (undefined :: a)
                b = bsize (undefined :: b) 
            in (max a b) - a
    padr _ = let
                a = bsize (undefined :: a)
                b = bsize (undefined :: b) 
            in (max a b) - b
    bsizf _ = undefined
    bsizs _ = undefined

instance (Types a, Types b) => Types (a :*: b) where
    bsize _ = let 
        a = bsize (undefined :: a)
        b = bsize (undefined :: b)
        in a + b
    padl _ = padl (undefined :: a)
    padr _ = padr (undefined :: a)
    bsizf _ = bsize (undefined :: a)
    bsizs _ = bsize (undefined :: b)