{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Simpl2BCC where
import Simplicity
import BCC

simpl2bcc :: Simpl i o -> Mph i o
simpl2bcc Iden          = Id
simpl2bcc (Comp f g)    = simpl2bcc g `O` simpl2bcc f 
simpl2bcc Unit          = Terminal
simpl2bcc (Injl f)      = Inj1 `O` (simpl2bcc f) 
simpl2bcc (Injr f)      = Inj2 `O` (simpl2bcc f)
simpl2bcc (Pair p q)    = Factor (simpl2bcc p) (simpl2bcc q)
simpl2bcc (Take f)      = simpl2bcc f `O` Fst
simpl2bcc (Drop f)      = simpl2bcc f `O` Snd
simpl2bcc (Case p q)    = Copair
                            (simpl2bcc p `O` prodFlip) 
                            (simpl2bcc q `O` prodFlip) 
                        `O` prodFlip