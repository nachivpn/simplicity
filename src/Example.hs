{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables,TypeOperators #-}
module Example where


import BCC
import Simplicity
import Debug.Trace
import SBM
import Simpl2SBM 
import BCC2SBM
import Simpl2BCC

type SBool = T :+: T

duplicate = Pair iden iden

iden :: Simpl SBool SBool MphId
iden = Iden

l = Lam $ Pair (Take iden) (Drop iden)

not' = Comp (Pair iden Unit) (Case (Injr Unit) (Injl Unit))

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

example1 :: (Types a, Types b) => Simpl a b n -> SBM (Maybe Bit)
example1 = example simpl2sbm

example2 :: (Types a, Types b) => Simpl a b n -> SBM (Maybe Bit)
example2 = example (bcc2sbm . simpl2bcc)  
