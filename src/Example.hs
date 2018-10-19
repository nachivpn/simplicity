{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE TypeOperators#-}
{-# LANGUAGE RankNTypes#-}
module Example where


import Mph
import Simplicity
import Debug.Trace
import SBM
import Simpl2SBM 
import BCC2SBM
import Simpl2BCC
import GHC.TypeLits
import Data.Proxy

type SBool = T :+: T

duplicate :: Simpl SBool (SBool :*: SBool)
duplicate = Pair Iden Iden

iden :: Simpl SBool SBool
iden = Iden

not' :: Simpl SBool SBool
not' = Comp (Pair Unit Iden) (Case (Injr Unit) (Injl Unit))

idenf :: (Types r, Types a) => 
    Simpl r (a :=>: a)
idenf = Lam (Drop Iden)

constf :: (Types r, Types a, Types b) => 
    Simpl r (a :=>: (b :=>: a))
constf = Lam (Lam (Take (Drop Iden)))

flipf :: (Types r, Types a, Types b, Types c) => 
    Simpl r (a :=>: (b :=>: c)) -> Simpl r b -> Simpl r a -> Simpl r c
flipf f b a = App (App f a) b 

example f se = do
    -- allocate bit for value
    newFrame 1
    -- write value
    write False
    -- move it to read frame (as that is place for input)
    moveFrame
    debugS "Machine state before: "
    -- allocate new frame for result value
    newFrame 1
    -- translate (and "run") simplicity expression to SBM instructions
    f se
    debugS "Machine state after: "
    -- move result to read frame
    moveFrame
    -- read!
    readFrame

debug = debugS ""
debugS str = get' >>= \s -> trace (str ++ show s) (return ())

example1 :: (Types a, Types b) => Simpl a b -> SBM (Maybe Bit)
example1 = example simpl2sbm

example2 :: (Types a, Types b) => Simpl a b -> SBM (Maybe Bit)
example2 = example (bcc2sbm . simpl2bcc)

z f = Drop f
s f = Take f


select' :: (Types a, Types r) => Simpl r (a :=>: (a :=>: (SBool :=>: a)))
select' =
  (Lam
    (Lam
      (Lam
        (Case
          (s . s $ z Iden)
          (s $ z Iden)
        ))))

true :: Types a => Simpl a SBool
true = Injr Unit

false :: Types a => Simpl a SBool
false = Injl Unit

idapp :: Simpl SBool SBool
idapp = App idenf true

constapp :: Simpl SBool SBool
constapp = App (App constf true) false

flipapp :: Simpl SBool SBool
flipapp = flipf constf true false

sapp :: Simpl SBool SBool
sapp = App (App (App select' (true)) false) true


selapp :: Simpl SBool T
selapp = App (App sel true) true

sel :: (Types r) => Simpl r (SBool :=>: (SBool :=>: T))
sel =
  Lam
   (Lam
     (Case
      (Unit)
      (Unit)))

type SNat = forall a. Types a => Simpl (a :=>: a) (a :=>: a)

zero :: SNat
zero = Lam (Drop Iden)

one :: SNat
one = Lam (App (Take Iden) (Drop Iden))

two :: SNat
two = Lam $ App (Take Iden) (App (Take Iden) (Drop Iden))

succ' :: SNat -> SNat
succ' n = Lam $ App (App (toLam n) s) (App s z)
    where
        s = Take Iden
        z = Drop Iden

toLam :: (Types a, Types b, Types r) => Simpl a b -> Simpl r (a :=>: b)
toLam s = Lam (Drop s)

loop :: forall a. Types a => Simpl a a -> SNat -> Simpl a a
loop f n = App (App (toLam n) (toLam f)) Iden

x :: Simpl SBool SBool
x = Comp true (loop not' one)

t :: Simpl SBool SBool 
t = (Comp (Pair Unit Unit) (Injl Unit))
