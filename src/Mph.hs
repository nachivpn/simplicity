{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables,ConstraintKinds #-}
{-# LANGUAGE UndecidableSuperClasses,TypeFamilies,AllowAmbiguousTypes #-}
module Mph where

import GHC.Exts (Constraint)
import Control.Category.Constrained

{-- Objects --}
data T
data a :*: b   -- Products
data a :+: b   -- Coproducts
data a :=>: b  -- Exponentials

{-- Morphisms --}
data Mph obj a b where
    -- Id, and composition (present in every category)
    Id        :: obj a => Mph obj a a
    O         :: (obj a, obj b, obj c) => Mph obj b c -> Mph obj a b -> Mph obj a c
    -- When having products
    Factor    ::  (obj a, obj b1, obj b2, obj (b1 :*: b2)) => Mph obj a b1 -> Mph obj a b2 -> Mph obj a (b1 :*: b2)
    -- Pairs proyections
    Fst       :: (obj a, obj b, obj (a :*: b)) => Mph obj (a :*: b) a
    Snd       :: (obj a, obj b, obj (a :*: b)) => Mph obj (a :*: b) b
    -- Terminal
    Terminal  :: obj a => Mph obj a T
    -- Coproducts
    Inj1      :: (obj a, obj b, obj (a :+: b)) => Mph obj a (a :+: b)
    Inj2      :: (obj a, obj b, obj (a :+: b)) => Mph obj b (a :+: b)
    Copair    :: (obj a, obj b, obj c, obj e, obj (e :*: a), obj (e :*: b), obj (a :+: b), obj (e :*: (a :+: b))) 
              => Mph obj (e :*: a) c -> Mph obj (e :*: b) c -> Mph obj (e :*: (a :+: b)) c
    Curry     :: (o a, o b, o r, o (r :*: a), o (a :=>: b))
              => Mph o (r :*: a) b -> Mph o r (a :=>: b)
    Eval      :: (o a, o b, o (a :=>: b), o ((a :=>: b) :*: a)) 
              => Mph o ((a :=>: b) :*: a) b

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
prodFlip :: (obj a, obj e, obj (a :*: e), obj (e :*: a)) => Mph obj (a :*: e) (e :*: a)
prodFlip = Factor Snd Fst 


{- Language of ABCCs and BCCs (in the style of constrained categories by Eliott)-}

type PairObj m a b = (Category m, Object m a, Object m b, Object m (Pair m a b))

class (Category m) => Cart m where
    type Pair m a b :: *
    type Terminal m :: *
    it  :: (u ~ Terminal m, Object m u, Object m a) => m a u
    fst' :: (PairObj m a b) => m (Pair m a b) a
    snd' :: (PairObj m a b) => m (Pair m a b) b
    (.*.)   :: (Object m a, PairObj m b1 b2) => m a b1 -> m a b2 -> m a (Pair m b1 b2)

type ExpObj m b c = (Category m, Object m b, Object m c, Object m (Exp m b c))

class Cart m => Closed m where
    type Exp m b a :: *
    eval      :: (ExpObj m b c, PairObj m (Exp m b c) b) => m (Pair m (Exp m b c) b) c
    curry'     :: (PairObj m a b, Object m c, ExpObj m b c) => m (Pair m a b) c -> m a (Exp m b c)

type CoPairObj m a b = (Category m, Object m a, Object m b, Object m (CoPair m a b))

-- Almost BiCartesian category
class (Category m) => ABiCart m where
    type CoPair m a b :: *
    inl    :: CoPairObj m a b => m a (CoPair m a b)
    inr    :: CoPairObj m a b => m b (CoPair m a b)
    (.+.)     :: (Object m z, CoPairObj m a b) => m a z -> m b z -> m (CoPair m a b) z

-- BiCartesian category (with initial object)
class (ABiCart m) => BiCart m where
    type Initial m :: *
    ge  :: (o ~ Initial m, Object m u, Object m a) => m o a

-- Almost BiCartesian Closed category
class (Cart m, ABiCart m, Closed m) => ABCC m where
    -- nothing to do!

-- BiCartesian Closed category
class (Cart m, BiCart m, Closed m) => BCC m where
    -- nothing to do!