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
            (p :: Simpl (e :*: a) d)
            (q :: Simpl (e :*: b) c)) = do
        let sizeOfE = bsize (undefined :: e)
        fwd sizeOfE             -- fwd to end of e
        mbit <- readFrame       -- read inl/inr flag bit to check if value is 'a' or 'b'
        bwd sizeOfE             -- bwd to beg of e
        case mbit of
            (Just False) -> do                  -- value is a
                let x = sizeOfE + 1 + padl (undefined :: (a :+: b))
                newFrame $ bsize (undefined :: (e :*: a)) -- new frame to write e :*: a
                copy sizeOfE                    -- copy e
                fwd x                           -- fwd to a
                copy $ bsize (undefined :: a)   -- copy a
                moveFrame                       -- move (e :*: a) from write to read
                simpl2sbm p                     -- run p (as read stack has value of type e :*: a)
                dropFrame                       -- drop frame containing e :*: a
                bwd x                           -- bwd to beginning
                return Nothing
            (Just True)  -> do                  -- value is b
                let x = sizeOfE + 1 + padr (undefined :: (a :+: b))
                newFrame $ bsize (undefined :: (e :*: b)) -- new frame to write e :*: b
                copy sizeOfE                    -- copy e
                fwd x                           -- fwd to b
                copy $ bsize (undefined :: b)   -- copy b
                moveFrame                       -- move (e :*: b) from write to read
                simpl2sbm q                     -- run q (as read stack has value of type e :*: b)
                dropFrame                       -- drop frame containing e :*: b
                bwd x                           -- bwd to beginning
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

