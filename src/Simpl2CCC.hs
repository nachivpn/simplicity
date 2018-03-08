{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Simpl2CCC where
import Simplicity
import CCC

type family Obj a where
    Obj ()          = T
    Obj (a :* b)    = Obj a :*: Obj b
    Obj (a :+ b)    = Obj a :+: Obj b

simpl2ccc :: Simpl i o -> Mph (Obj i) (Obj o)
simpl2ccc Iden          = Id
simpl2ccc (Comp f g)    = simpl2ccc g `O` simpl2ccc f 
simpl2ccc Unit          = Terminal
simpl2ccc (Injl f)      = Inj1CCC `O` (simpl2ccc f) 
simpl2ccc (Injr f)      = Inj2CCC `O` (simpl2ccc f)
simpl2ccc (Pair p q)    = Factor (simpl2ccc p) (simpl2ccc q)
simpl2ccc (Take f)      = simpl2ccc f `O` Fst
simpl2ccc (Drop f)      = simpl2ccc f `O` Snd
simpl2ccc (Case p q)    = Copair
                            (simpl2ccc p `O` prodFlip) 
                            (simpl2ccc q `O` prodFlip) 
                        `O` prodFlip