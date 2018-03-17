{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables,TypeOperators #-}
module Example where


import CCC
import Simplicity
import Simpl2SBM 
import Debug.Trace
import SBM

type SBool = T :+: T

duplicate :: Simpl SBool (SBool :*: SBool)
duplicate = Pair Iden Iden

iden :: Simpl SBool SBool
iden = Iden

not' :: Simpl SBool SBool
not' = Comp (Pair Iden Unit) (Case (Injr Unit) (Injl Unit))

example se = do
        newFrame 1
        write False
        moveFrame
        debugS "Machine state before: "
        newFrame 1
        (simpl2sbm se)
        debugS "Machine state after: "
        moveFrame
        readFrame

debug = debugS ""
debugS str = get' >>= \s -> trace (str ++ show s) (return ())

