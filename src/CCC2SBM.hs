{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables,TypeOperators #-}
module CCC2SBM where

import CCC
import SBM

c2s :: (Types a, Types b) => Mph a b -> [Instr]
c2s (Id :: Mph a b) = [Copy $ bsize (undefined :: a)]
c2s ((f :: Mph b c) `O` (Snd :: Mph ab b)) =
        [Fwd (bsizf (undefined :: ab))] ++ c2s f ++ [Bwd (bsizf (undefined :: ab))]
c2s ((f :: Mph c b) `O` (g :: Mph a c))  =
        [NewFrame $ bsize (undefined :: b)] ++ c2s g ++ [MoveFrame] ++ c2s f ++ [DropFrame]
c2s (Terminal) = [Nop]
c2s (Inj1CCC :: Mph a bc) = [Write False, Skip $ padl (undefined :: bc)]
c2s (Inj2CCC :: Mph a bc) = [Write True, Skip $ padr (undefined :: bc)]
c2s (Factor p q) = c2s p ++ c2s q
c2s (Fst) = []
