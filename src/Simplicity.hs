{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Simplicity where

import Data.Either

type (:*) a b = (a,b)
type (:+) a b = Either a b

data Simpl i o where
    Iden :: Simpl a a
    Comp :: Simpl a b -> Simpl b c -> Simpl a c
    Unit :: Simpl a ()
    Injl :: Simpl a b -> Simpl a (b :+ c)
    Injr :: Simpl a c -> Simpl a (b :+ c)
    Case :: Simpl (a :* c) d -> Simpl (b :* c) d -> Simpl ((a :+ b) :* c) d
    Pair :: Simpl a b -> Simpl a c -> Simpl a (b :* c)
    Take :: Simpl a c -> Simpl (a :* b) c
    Drop :: Simpl b c -> Simpl (a :* b) c

