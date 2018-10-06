{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}


module Simpl2BCC where
import Simplicity
import BCC

bcc2simpl ::  Mph Types i o -> Simpl i o 
bcc2simpl Id            = Iden
bcc2simpl (f `O` g)     = Comp (bcc2simpl g) (bcc2simpl f)
bcc2simpl Terminal      = Unit
bcc2simpl Inj1          = Injl Iden
bcc2simpl Inj2          = Injr Iden
bcc2simpl (Factor f g)  = Pair (bcc2simpl f) (bcc2simpl g)
bcc2simpl (Copair f g)  = Comp simplflip (Case (bcc2simpl' f) (bcc2simpl' g))
    where
    simplflip = bcc2simpl $ prodFlip @Types
    bcc2simpl' x = bcc2simpl (x `O` prodFlip)