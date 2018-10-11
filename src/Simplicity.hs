{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE TypeFamilies,FlexibleInstances,TypeApplications#-}

module Simplicity where

import Data.Either
import Mph
import qualified Control.Category.Constrained as ConCat

data Simpl i o where
    Iden :: Types a => Simpl a a
    Comp :: (Types a, Types b, Types c) => Simpl a b -> Simpl b c -> Simpl a c
    Unit :: Types a => Simpl a T
    Injl :: (Types a, Types b, Types c) => Simpl a b -> Simpl a (b :+: c)
    Injr :: (Types a, Types b, Types c) => Simpl a c -> Simpl a (b :+: c)
    Case :: (Types a, Types b, Types c, Types d) => Simpl (a :*: c) d -> Simpl (b :*: c) d -> Simpl ((a :+: b) :*: c) d
    Pair :: (Types a, Types b, Types c) => Simpl a b -> Simpl a c -> Simpl a (b :*: c)
    Take :: (Types a, Types b, Types c) => Simpl a c -> Simpl (a :*: b) c
    Drop :: (Types a, Types b, Types c) => Simpl b c -> Simpl (a :*: b) c
    Lam  :: (Types r, Types a, Types b) => Simpl (r :*: a) b -> Simpl r (a :=>: b)
    App  :: (Types r, Types a, Types b) => Simpl r (a :=>: b) -> Simpl r a -> Simpl r b

type family Hask i :: * where
  Hask T          = () 
  Hask (b :*: c)  = (Hask b, Hask c)
  Hask (b :+: c)  = Either (Hask b) (Hask c)
  Hask (b :=>: c) = (Hask b) -> (Hask c)

-- denotational semantics
ds :: Simpl i o -> (Hask i -> Hask o)
ds Iden       = id
ds (Comp s t) = ds t . ds s
ds Unit       = \_ -> ()
ds (Injl t)   = Left . ds t
ds (Injr t)   = Right . ds t
ds (Pair s t) = \x -> (ds s x, ds t x)
ds (Take t)   = ds t . fst
ds (Drop t)   = ds t . snd
ds (Lam f)    = curry (ds f)
ds (App f g)  = \a -> let f' = ds f; g' = ds g; b = g' a in f' a b
ds (Case s t) = \x -> case x of (Left a,c) -> ds s (a,c); (Right a,c) -> ds t (a,c); 

-- |Types is a type class which is implemnted only by Simplicity types
class Types a where
  bsize :: a -> Int
  
class SumTypes a where
  padl  :: a -> Int
  padr  :: a -> Int

class ProductTypes a where
  bsizf :: a -> Int
  bsizs :: a -> Int

instance Types T where
  bsize _ = 0

instance (Types a, Types b) => Types (a :+: b) where
  bsize _ = let 
              a = bsize (undefined :: a)
              b = bsize (undefined :: b) 
          in 1 + max a b

instance (Types a, Types b) => Types (a :*: b) where
  bsize _ = let 
      a = bsize (undefined :: a)
      b = bsize (undefined :: b)
      in a + b

instance (Types a, Types b) => Types (a :=>: b) where

instance (Types a, Types b) => ProductTypes (a :*: b) where
  bsizf _ = bsize (undefined :: a)
  bsizs _ = bsize (undefined :: b)

instance (Types a, Types b) => SumTypes (a :+: b) where
  padl _ = let
              a = bsize (undefined :: a)
              b = bsize (undefined :: b) 
          in (max a b) - a
  padr _ = let
              a = bsize (undefined :: a)
              b = bsize (undefined :: b) 
          in (max a b) - b

instance ConCat.Category Simpl where
    type Object Simpl o = Types o
    id = Iden
    (.) = flip Comp

instance Cart Simpl where
    type Pair Simpl a b = a :*: b
    type Terminal Simpl = T
    it = Unit
    fst' = (Take Iden)
    snd' = (Drop Iden)
    (.*.) = Pair

instance Closed Simpl where
    type Exp Simpl a b = (a :=>: b)
    curry' = Lam
    eval  = App (Take Iden) (Drop Iden)

instance ABiCart Simpl where
    type CoPair Simpl a b = (a :+: b)
    inl = Injl Iden
    inr = Injr Iden
    f .+. g = Comp (Pair Iden Unit) (Case (Take f) (Take g))

instance ABCC Simpl where
  -- nothing to do!

-- partial as it stands
betared :: Simpl i o -> Simpl i o
betared (App (Lam f) x) = Comp (Pair Iden x) f
betared (Comp (Pair x y) (Take f)) = Comp x f
betared (Comp (Pair x y) (Drop f)) = Comp y f
betared (Comp f Iden) = f
betared (Comp Iden f) = f
betared (Comp f Unit) = Unit