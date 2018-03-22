{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables,ConstraintKinds #-}
module BCC where

{-- Objects --}
data T
data a :*: b   -- Products
data a :+: b   -- Coproducts

{-- Morphisms --}
data Mph obj a b where
  -- Id, and composition (present in every category)
    Id       :: obj a => Mph obj a a
    O        :: (obj a, obj b, obj c) => Mph obj b c -> Mph obj a b -> Mph obj a c
    -- When having products
    Factor   ::  (obj a, obj b1, obj b2) => Mph obj a b1 -> Mph obj a b2 -> Mph obj a (b1 :*: b2)
    -- Pairs proyections
    Fst      :: (obj a, obj b) => Mph obj (a :*: b) a
    Snd      :: (obj a, obj b) => Mph obj (a :*: b) b
    -- Terminal
    Terminal :: Mph r a T
    -- Coproducts
    Inj1 :: (obj a, obj b) => Mph obj a (a :+: b)
    Inj2 :: (obj a, obj b) => Mph obj b (a :+: b)
    Copair  :: (obj a, obj b, obj c, obj e) => Mph obj (e :*: a) c -> Mph obj (e :*: b) c -> Mph obj (e :*: (a :+: b)) c

-- CCC to String
toString :: (Mph obj a b) -> String
toString Id             = "Id"
toString (O g f)        = "(" ++ toString g ++ " o " ++ toString f ++ ")"
toString (Factor f g)   = "<" ++ toString f ++ ", " ++ toString g ++ ">"
toString Fst            = "Fst"
toString Snd            = "Snd"
toString Terminal       = "Terminal"
toString Inj1           = "Inj1"
toString Inj2           = "Inj2"
toString (Copair f g)   = "[" ++ toString f ++ ", " ++ toString g ++ "]"

-- Products are symmetric wrt isomorphism
prodFlip :: (obj a, obj e, obj (a :*: e)) => Mph obj (a :*: e) (e :*: a)
prodFlip = Factor Snd Fst



-- |Types is a type class which is implemnted only by Simplicity types
class Types a where
  bsize :: a -> Int
  padl  :: a -> Int
  padr  :: a -> Int
  bsizf :: a -> Int
  bsizs :: a -> Int
  
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