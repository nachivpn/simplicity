{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Arbitrary where

import Mph
import Simplicity
import Example
import Data.Proxy
import Test.QuickCheck

-- "freely" generating compositions using various strategies
fgen :: (Types a, Types b, Types c) => Simpl a b -> Simpl b c -> [Simpl a c]
fgen x y =
  Comp x y
  : go x y -- this is only ONE strategy, TODO more!
  where
    go x y = Comp (Pair x x) (Drop y) : go x y

instance Arbitrary (Simpl T T) where
  arbitrary = do
    let comp = Comp
          <$> (arbitrary :: Gen (Simpl T T))
          <*> (arbitrary :: Gen (Simpl T T))
    frequency [(5,return Iden),(1,comp),(4,return Unit)]

instance (Types a, Types b,
          Arbitrary (Simpl a T), Arbitrary (Simpl b T))
  => Arbitrary (Simpl (a :*: b) T) where
  arbitrary = oneof [Take <$> arbitrary, Drop  <$> arbitrary]

instance (Types a, Types b,
          Arbitrary (Simpl T a), Arbitrary (Simpl T b))
  => Arbitrary (Simpl T (a :*: b)) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Types a, Types b, Types c, Types d,
          Arbitrary (Simpl (a :*: b) c), Arbitrary (Simpl (a :*: b) d))
  => Arbitrary (Simpl (a :*: b) (c :*: d)) where
  arbitrary = Pair <$> arbitrary <*> arbitrary
  
instance (Types a, Types b,
          Arbitrary (Simpl T a), Arbitrary (Simpl T b))
  => Arbitrary (Simpl T (a :+: b)) where
  arbitrary = oneof [Injl <$> arbitrary, Injr <$> arbitrary]

instance (Types a, Types b,
          Arbitrary (Simpl a T), Arbitrary (Simpl b T))
  => Arbitrary (Simpl (a :+: b) T) where
  arbitrary = Comp (Pair Unit Iden) <$>
    (Case
     <$> (Drop <$> arbitrary)
     <*> (Drop <$> arbitrary))

instance (Types a, Types b, Types c, Types d,
          Arbitrary (Simpl a (c :+: d)), Arbitrary (Simpl b (c :+: d)))
  => Arbitrary (Simpl (a :+: b) (c :+: d)) where
  arbitrary = return (Comp (Pair Unit Iden))
    <*> (Case
         <$> (Drop <$> (arbitrary))
         <*> (Drop <$> (arbitrary)))


instance (Types a, Types b, Types c, Types d)
  => Arbitrary (Simpl (a :+: b) (c :*: d)) where
  --TODO

instance (Types a, Types b, Types c, Types d)
  => Arbitrary (Simpl (a :*: b) (c :+: d)) where
  --TODO
  
