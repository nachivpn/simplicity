{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE TypeFamilies,FlexibleInstances,TypeApplications#-}

module Simplicity where

import Debug.Trace
import Data.Either
import Data.Maybe
import Mph
import qualified Control.Category.Constrained as ConCat

data Simpl i o where
    Iden :: Types a => Simpl a a
    Comp :: (Types a, Types b, Types c) => Simpl a b -> Simpl b c -> Simpl a c
    Unit :: Types a => Simpl a T
    Injl :: (Types a, Types b, Types c) => Simpl a b -> Simpl a (b :+: c)
    Injr :: (Types a, Types b, Types c) => Simpl a c -> Simpl a (b :+: c)
    Case :: (Types a, Types b, Types r, Types d) =>
            Simpl (r :*: a) d -> Simpl (r :*: b) d -> Simpl (r :*: (a :+: b)) d
    Pair :: (Types a, Types b, Types c) => Simpl a b -> Simpl a c -> Simpl a (b :*: c)
    Take :: (Types a, Types b, Types c) => Simpl a c -> Simpl (a :*: b) c
    Drop :: (Types a, Types b, Types c) => Simpl b c -> Simpl (a :*: b) c
    Lam  :: (Types r, Types a, Types b) => Simpl (r :*: a) b -> Simpl r (a :=>: b)
    App  :: (Types r, Types a, Types b) => Simpl r (a :=>: b) -> Simpl r a -> Simpl r b

instance Show (Simpl i o) where
    show Iden         = "Iden"
    show (Comp f g)   = "(Comp " ++ show f ++ " " ++ show g ++ ")"
    show (Pair f g)   = "<" ++ show f ++ "," ++ show g ++ ">"
    show (Take f)     = "(Take " ++ show f ++ ")"
    show (Drop f)     = "(Drop " ++ show f ++ ")"
    show Unit         = "Unit"
    show (Injl f)     =  "(Injl " ++ show f ++ ")"
    show (Injr f)     =  "(Injr " ++ show f ++ ")"
    show (Case f g)   = "(Case " ++ show f ++ "; " ++ show g ++ ")"
    show (Lam f)      = "(Lam " ++ show f ++ ")"
    show (App f x)    =  "(App " ++ show f ++ " " ++ show x ++ ")"
    
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
ds (Case s t) = \x -> case x of (r,Left a) -> ds s (r,a); (r,Right b) -> ds t (r,b); 

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
    f .+. g = Comp (Pair Unit Iden) (Case (Drop f) (Drop g))

instance ABCC Simpl where
  -- nothing to do!

beta :: Simpl i o -> Maybe (Simpl i o)
-- app elimination
beta (App (Lam f) x) = Just $ Comp (Pair Iden x) f

-- pushing substitution in
beta (Comp s (App f x)) = Just $ App (Comp s f) (Comp s x)
beta (Comp s (Lam f)) = Just $ Lam (Comp (Pair (Take s) (Drop Iden)) f)

-- indexing / pair elimination
beta (Comp (Pair x _) (Take f)) = Just $ Comp x f
beta (Comp (Pair _ y) (Drop f)) = Just $ Comp y f

-- identity
beta (Comp s Iden) = Just $ s
beta (Comp Iden s) = Just $ s

-- unit
beta (Comp _ Unit) = Just $ Unit

--- case elimination
beta (Comp (Pair g (Injl f)) (Case p _)) = Just $ Comp (Pair g f) p
beta (Comp (Pair g (Injr f)) (Case _ q)) = Just $ Comp (Pair g f) q

-- associativity of composition
beta (Comp s (Comp t u)) = Just $ Comp (Comp s t) u

-- subst composition
beta (Comp t (Pair s a)) = Just $ Pair (Comp t s) (Comp t a)

-- no more re-writes!
beta _ = Nothing


reducible = isJust . beta'

beta' :: Simpl i o -> Maybe (Simpl i o)
beta' t@(App f x)
  | reducible f = App <$> beta' f <*> return x
  | reducible x = App f <$> beta' x 
  | otherwise = beta t
beta' t@(Lam f)
  | reducible f = Lam <$> beta' f
  | otherwise   = beta t
beta' t@(Pair p q)
  | reducible p = Pair <$> beta' p <*> return q
  | reducible q = Pair p <$> beta' q
  | otherwise   = beta t
beta' t@(Comp f g)
  | reducible f = Comp <$> beta' f <*> return g
  | reducible g = Comp f <$> beta' g
  | otherwise   = beta t
beta' t@(Case p q)
  | reducible p = Case <$> beta' p <*> return q
  | reducible q = Case p <$> beta' q
  | otherwise   = beta t
beta' t@(Injl f)
  | reducible f = Injl <$> beta' f
  | otherwise   = beta t 
beta' t@(Injr f)
  | reducible f = Injr <$> beta' f
  | otherwise   = beta t 
beta' t@(Take f)
  | reducible f = Take <$> beta' f
  | otherwise   = beta t 
beta' t@(Drop f)
  | reducible f = Drop <$> beta' f
  | otherwise   = beta t 
beta' f = beta f

beta_ f =
  let mf' = beta' f
  in case mf' of
    (Just f') -> trace (show f') $ beta_ f'
    (Nothing) -> f
