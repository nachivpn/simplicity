{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE TypeFamilies#-}

module Denotational where

import BCC
import Simplicity

type family Hask i :: * where
  Hask T          = () 
  Hask (b :*: c)  = (Hask b, Hask c)
  Hask (b :+: c)  = Either (Hask b) (Hask c)
  Hask (b :=>: c) = (Hask b) -> (Hask c)

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

dsM :: Mph obj i o -> (Hask i -> Hask o)
dsM Id              = id
dsM (O g f)         = dsM g . dsM f
dsM (Factor p q)    = \x -> (dsM p x, dsM q x)
dsM Fst             = fst
dsM Snd             = snd
dsM Terminal        = \_ -> ()
dsM Inj1            = Left
dsM Inj2            = Right
dsM (Curry f)       = curry (dsM f)
dsM Eval            = uncurry ($)
dsM (Copair p q)    = \(e,x) -> case x of Left a -> dsM p (e,a); Right b -> dsM q (e,b)