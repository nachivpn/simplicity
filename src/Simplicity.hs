{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators,ScopedTypeVariables #-}

module Simplicity where

import Data.Either
import CCC

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