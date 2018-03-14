{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, TypeInType, AllowAmbiguousTypes,FlexibleContexts #-}
module CCC2SBM where

import CCC
import SBM

-- Approach 1 (Types type class)

c2s :: (Types a, Types b) => Mph a b -> [Instr]
c2s (Id :: Mph a b) = [Copy $ size (undefined :: a)]
c2s ((f :: Mph c b) `O` (g :: Mph a c))  =
        [NewFrame $ size (undefined :: b)] ++ c2s g ++ [MoveFrame] ++ c2s f ++ [DropFrame]
c2s (Terminal) = [Nop]
c2s (Inj1CCC :: Mph a bc) = [Write False, Skip $ padl (undefined :: bc)]
c2s (Inj2CCC :: Mph a bc) = [Write True, Skip $ padr (undefined :: bc)]