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
    (simpl2sbm se)
    debugS "Machine state after: "
    -- move result to read frame
    moveFrame
    -- read!
    readFrame

debug = debugS ""
debugS str = get' >>= \s -> trace (str ++ show s) (return ())

