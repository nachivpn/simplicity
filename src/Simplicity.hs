{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE TypeFamilies#-}

module Simplicity where

import Data.Either
import BCC

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
  bsize _ = 0

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