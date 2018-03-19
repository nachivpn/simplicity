{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables,TypeOperators #-}
module BCC2SBM where

import BCC
import SBM
import Simplicity

bcc2sbm :: (Types a, Types b) => Mph a b -> SBM (Maybe Bit)
bcc2sbm (Id :: Mph a b) = do
        copy (bsize (undefined :: a))
        return Nothing
bcc2sbm ((g :: Mph b c) `O` (f :: Mph a b)) = do
        newFrame $ bsize (undefined :: b)
        bcc2sbm f
        moveFrame
        bcc2sbm g
        dropFrame
        return Nothing
bcc2sbm (Terminal)      = nop >> return Nothing
bcc2sbm (Inj1 :: Mph a ab)          = do
        write False
        skip $ padl (undefined :: ab)
        copy $ bsize (undefined :: a)
        return Nothing
bcc2sbm (Inj2 :: Mph b ab)          = do
        write True
        skip $ padl (undefined :: ab)
        copy $ bsize (undefined :: b)
        return Nothing
bcc2sbm (Copair -- :: Mph (e :*: (a :+: b)) c
            (p :: Mph (e :*: a) c)
            (q :: Mph (e :*: b) c)) = do
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
                bcc2sbm p                       -- run p (as read stack has value of type e :*: a)
                dropFrame                       -- drop frame containing e :*: a
                bwd x                           -- bwd to beginning
            (Just True)  -> do                  -- value is b
                let x = sizeOfE + 1 + padr (undefined :: (a :+: b))
                newFrame $ bsize (undefined :: (e :*: b)) -- new frame to write e :*: b
                copy sizeOfE                    -- copy e
                fwd x                           -- fwd to b
                copy $ bsize (undefined :: b)   -- copy b
                moveFrame                       -- move (e :*: b) from write to read
                bcc2sbm q                       -- run q (as read stack has value of type e :*: b)
                dropFrame                       -- drop frame containing e :*: b
                bwd x                           -- bwd to beginning
        return Nothing
bcc2sbm (Factor -- :: Mph a (b :*: c)
                (p :: Mph a b)
                (q :: Mph a c)) = do
        bcc2sbm p
        bcc2sbm q
        return Nothing
bcc2sbm (Fst :: Mph ab a)     = do
    copy (bsize (undefined :: a))
    return Nothing
bcc2sbm (Snd :: Mph ab b)     = do
    fwd (bsizf (undefined :: ab))
    copy (bsize (undefined :: b))
    bwd (bsizf (undefined :: ab))
    return Nothing