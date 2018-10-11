{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables,TypeOperators #-}
module Simpl2SBM where

import Mph
import SBM
import Simplicity
import Simpl2BCC

simpl2sbm :: Simpl a b -> SBM (Maybe Bit)
simpl2sbm (Iden :: Simpl a b) = do
        copy (bsize (undefined :: a))
        return Nothing
simpl2sbm (Comp (f :: Simpl a b) (g :: Simpl b c)) = do
        newFrame $ bsize (undefined :: b)
        simpl2sbm f
        moveFrame
        simpl2sbm g
        dropFrame
        return Nothing
simpl2sbm (Unit) = nop >> return Nothing
simpl2sbm (Injl (t :: Simpl a b) :: Simpl a bc) = do
        write False
        skip $ padl (undefined :: bc)
        simpl2sbm t
        return Nothing
simpl2sbm (Injr (t :: Simpl a c) :: Simpl a bc) = do
        write True
        skip $ padr (undefined :: bc)
        simpl2sbm t
        return Nothing
simpl2sbm (Case -- :: Simpl ((a :+: b) :*: c) d
            (s :: Simpl (a :*: c) d)
            (t :: Simpl (b :*: c) d)) = do
        mbit <- readFrame
        case mbit of
            (Just False) -> do  
                fwd $ 1 + padl (undefined :: (a :+: b))
                simpl2sbm s  
                bwd $ 1 + padl (undefined :: (a :+: b)) 
                return Nothing
            (Just True)  -> do 
                fwd $ padr (undefined :: (a :+: b)) 
                simpl2sbm t 
                bwd $ 1 + padr (undefined :: (a :+: b))
                return Nothing
simpl2sbm (Pair
                (p :: Simpl a b)
                (q :: Simpl a c)) = do
        simpl2sbm p            
        simpl2sbm q
        return Nothing
simpl2sbm (Take t) = simpl2sbm t
simpl2sbm (Drop t :: Simpl ab c)     = do
        fwd (bsizf (undefined :: ab))
        simpl2sbm t
        bwd (bsizf (undefined :: ab))
        return Nothing

