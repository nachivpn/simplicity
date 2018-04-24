{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables,TypeOperators #-}
module Simpl2SBM where

import BCC
import SBM
import Simplicity
import Simpl2BCC

simpl2sbm :: Simpl a b n -> SBM (Maybe Bit)
simpl2sbm (Iden :: Simpl a b n) = do
        copy (bsize (undefined :: a))
        return Nothing
simpl2sbm (Comp (f :: Simpl a b n) (g :: Simpl b c m)) = do
        newFrame $ bsize (undefined :: b)
        simpl2sbm f
        moveFrame
        simpl2sbm g
        dropFrame
        return Nothing
simpl2sbm (Unit) = nop >> return Nothing
simpl2sbm (Injl (t :: Simpl a b n) :: Simpl a bc m) = do
        write False
        skip $ padl (undefined :: bc)
        simpl2sbm t
        return Nothing
simpl2sbm (Injr (t :: Simpl a c n) :: Simpl a bc m) = do
        write True
        skip $ padr (undefined :: bc)
        simpl2sbm t
        return Nothing
simpl2sbm (Case -- :: Simpl ((a :+: b) :*: c) d
            (s :: Simpl (a :*: c) d n)
            (t :: Simpl (b :*: c) d m)) = do
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
                (p :: Simpl a b n)
                (q :: Simpl a c m)) = do
        simpl2sbm p            
        simpl2sbm q
        return Nothing
simpl2sbm (Take t) = simpl2sbm t
simpl2sbm (Drop t :: Simpl ab c n)     = do
        fwd (bsizf (undefined :: ab))
        simpl2sbm t
        bwd (bsizf (undefined :: ab))
        return Nothing

