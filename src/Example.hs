{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE TypeOperators#-}
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
not' = Comp (Pair Iden Unit) (Case (Injr Unit) (Injl Unit))

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

---------------------------------------------------
-- Experiments with environment
---------------------------------------------------

fl :: (Types a, Types b) => Simpl (a :*: b) (b :*: a)
fl = Pair (Drop Iden) (Take Iden)

select :: (Types a) => Simpl T (a :=>: (a :=>: (SBool :=>: a)))
select =
  (Lam
    (Lam
      (Lam
        (Comp
         fl
         (Case
           (Drop (Take (Drop Iden)))
           (Drop (Drop Iden))
         )))))

z f = Drop f
s f = Take f

case' :: (Types a, Types b, Types r, Types d) =>
  Simpl (r :*: a) d -> Simpl (r :*: b) d -> Simpl (r :*: (a :+: b)) d
case'  p q = Comp fl (Case (Comp fl p) (Comp fl q)) 
  
select' :: (Types a, Types r) => Simpl r (a :=>: (a :=>: (SBool :=>: a)))
select' =
  (Lam
    (Lam
      (Lam
        (case'
          (s . s $ z Iden)
          (s $ z Iden)
        ))))

