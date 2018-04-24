{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators,ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Simplicity where

import Data.Either
import BCC

data Simpl i o (n :: Nat) where
    Iden :: Types a => Simpl a a MphId
    Comp :: (Types a, Types b, Types c, IsNat n, IsNat m) => Simpl a b n -> Simpl b c m -> Simpl a c (Sum MphId (Sum m n))
    Unit :: Types a => Simpl a T MphId
    Injl :: (Types a, Types b, Types c, IsNat n) => Simpl a b n -> Simpl a (b :+: c) (Sum MphId (Sum MphId n))
    Injr :: (Types a, Types b, Types c, IsNat n) => Simpl a c n -> Simpl a (b :+: c) (Sum MphId (Sum MphId n))
    Case :: (Types a, Types b, Types c, Types d, IsNat n, IsNat m)
         => Simpl (a :*: c) d n -> Simpl (b :*: c) d m
         -> Simpl ((a :+: b) :*: c) d (Sum MphId (Sum (Sum MphId (Sum (Sum MphId (Sum n (Sum MphId (Sum MphId MphId))))
                                                                      (Sum MphId (Sum m (Sum MphId (Sum MphId MphId))))))
                                                           (Sum MphId (Sum MphId MphId))))
    Pair :: (Types a, Types b, Types c, IsNat n, IsNat m) => Simpl a b n -> Simpl a c m -> Simpl a (b :*: c) (Sum MphId (Sum n m))
    Take :: (Types a, Types b, Types c, IsNat n) => Simpl a c n -> Simpl (a :*: b) c (Sum MphId (Sum n MphId))
    Drop :: (Types a, Types b, Types c, IsNat n) => Simpl b c n -> Simpl (a :*: b) c (Sum MphId (Sum n MphId))
    Lam  :: (Types a, Types b, Types c, IsNat n) => Simpl (a :*: b) c n -> Simpl a (Exp b c n) (Sum MphId n)
    App  :: (Types a, Types b, Types c, IsNat n, IsNat m, IsNat x) => Simpl a (Exp b c x) n -> Simpl a b m -> Simpl a c (Sum MphId (Sum MphId (Sum MphId (Sum n m))))

