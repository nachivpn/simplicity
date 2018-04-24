{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables, ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module BCC where

data Nat = Max Nat Nat | Sum Nat Nat | Z

{-- Objects --}
data T
data a :*: b   -- Products
data a :+: b   -- Coproducts
data Exp a b (n::Nat)   -- Exponentials

type family Size a :: Nat
type instance Size T = Z
type instance Size (a :*: b) = Sum (Size a) (Size b)
type instance Size (a :+: b) = Sum (Max (Size a) (Size b)) Z
type instance Size (Exp a b n) = n

-- Nat singleton
data SNat (n :: Nat) where
  SZero :: SNat Z
  SSucc :: SNat n -> SNat m -> SNat (Sum n m)

class IsNat (a :: Nat) where
  toInt :: SNat a -> Int

instance IsNat Z where
  toInt _ = 0

instance (IsNat n, IsNat m) => IsNat (Sum n m) where
  toInt _ = 1 + toInt (undefined :: (SNat n)) + toInt (undefined :: (SNat m))

instance (IsNat n, IsNat m) => IsNat (Max n m) where
  toInt _ = (max (toInt (undefined :: (SNat n))) (toInt (undefined :: (SNat m))))

-- |Types is a type class which is implemnted only by Simplicity types
class IsNat (Size a) => Types a where
  bsize :: a -> Int
  padl  :: a -> Int
  padr  :: a -> Int
  bsizf :: a -> Int
  bsizs :: a -> Int
  
type One = Sum Z Z
type Two = Sum One Z
type Three = Sum Two Z

type MphId = Sum Three Z 

{-- Morphisms --}
data Mph obj a b (n::Nat) where
  -- Id, and composition (present in every category)
    Id       :: obj a => Mph obj a a MphId
    O        :: (obj a, obj b, obj c, IsNat n, IsNat m)
             => Mph obj b c n -> Mph obj a b m
             -> Mph obj a c (Sum MphId (Sum n m))
    -- When having products
    Factor   :: (obj a, obj b1, obj b2, IsNat n, IsNat m)
             => Mph obj a b1 n -> Mph obj a b2 m
             -> Mph obj a (b1 :*: b2) (Sum MphId (Sum n m))
    -- Pairs proyections
    Fst      :: (obj a, obj b) => Mph obj (a :*: b) a MphId
    Snd      :: (obj a, obj b) => Mph obj (a :*: b) b MphId
    -- Terminal
    Terminal :: obj a => Mph obj a T MphId
    -- Coproducts
    Inj1     :: (obj a, obj b) => Mph obj a (a :+: b) MphId
    Inj2     :: (obj a, obj b) => Mph obj b (a :+: b) MphId
    Copair   :: (obj a, obj b, obj c, obj e, IsNat n, IsNat m) => Mph obj (e :*: a) c n
             -> Mph obj (e :*: b) c m 
             -> Mph obj (e :*: (a :+: b)) c (Sum MphId (Sum n m))
    -- Exponentials
    Curry    :: (obj a, obj b, obj c, IsNat n) => Mph obj (a :*: b) c n -> Mph obj a (Exp b c n) (Sum MphId n)
    Eval     :: (obj b, obj c, IsNat x) => Mph obj ((Exp b c x) :*: b) c MphId

-- CCC to String
toString :: (Mph o r a n) -> String
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
prodFlip :: (obj a, obj e, obj (a :*: e), obj (e :*: a), IsNat (Size (e :*: a)), IsNat (Size e), IsNat (Size a)) => Mph obj (a :*: e) (e :*: a) (Sum MphId (Sum MphId MphId))
prodFlip = Factor Snd Fst


instance Types T where
  bsize _ = 0
  padl  _ = undefined
  padr _  = undefined
  bsizf _ = undefined
  bsizs _ = undefined

instance (Types a, Types b) => Types (a :+: b) where
  bsize _ = toInt (undefined :: (SNat (Size (a :+: b))))
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
  bsize _ = toInt (undefined :: SNat (Size (a :*: b)))
  padl _ = padl (undefined :: a)
  padr _ = padr (undefined :: a)
  bsizf _ = bsize (undefined :: a)
  bsizs _ = bsize (undefined :: b)

instance (Types a, Types b, IsNat n) => Types (Exp a b n) where
  bsize _ = toInt (undefined :: (SNat n))
  padl  _ = undefined
  padr _  = undefined
  bsizf _ = undefined
  bsizs _ = undefined

