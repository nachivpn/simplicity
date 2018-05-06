{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables,TypeOperators #-}
module BCC2SBM where

import BCC
import SBM
import Simplicity

bcc2sbm :: Mph Types a b -> SBM ()
bcc2sbm (Id :: Mph Types a b) = do
        copy (bsize (undefined :: a))
bcc2sbm ((g :: Mph Types b c) `O` (f :: Mph Types a b)) = do
        newFrame $ bsize (undefined :: b)
        bcc2sbm f
        moveFrame
        bcc2sbm g
        dropFrame
bcc2sbm (Terminal)      = nop
bcc2sbm (Inj1 :: Mph Types a ab)          = do
        write False
        skip $ padl (undefined :: ab)
        copy $ bsize (undefined :: a)
bcc2sbm (Inj2 :: Mph Types b ab)          = do
        write True
        skip $ padr (undefined :: ab)
        copy $ bsize (undefined :: b)
bcc2sbm (Copair -- :: Mph (e :*: (a :+: b)) c
            (p :: Mph Types (e :*: a) c)
            (q :: Mph Types (e :*: b) c)) = do
        let sizeOfE = bsize (undefined :: e)
        fwd sizeOfE             -- fwd to end of e
        mbit <- readFrame       -- read inl/inr flag bit to check if value is 'a' or 'b'
        bwd sizeOfE             -- bwd to beg of e
        case mbit of
            False -> do                  -- value is a
                let x = sizeOfE + 1 + padl (undefined :: (a :+: b))
                newFrame $ bsize (undefined :: (e :*: a)) -- new frame to write e :*: a
                copy sizeOfE                    -- copy e
                fwd x                           -- fwd to a
                copy $ bsize (undefined :: a)   -- copy a
                moveFrame                       -- move (e :*: a) from write to read
                bcc2sbm p                       -- run p (as read stack has value of type e :*: a)
                dropFrame                       -- drop frame containing e :*: a
                bwd x                           -- bwd to beginning
            True  -> do                  -- value is b
                let x = sizeOfE + 1 + padr (undefined :: (a :+: b))
                newFrame $ bsize (undefined :: (e :*: b)) -- new frame to write e :*: b
                copy sizeOfE                    -- copy e
                fwd x                           -- fwd to b
                copy $ bsize (undefined :: b)   -- copy b
                moveFrame                       -- move (e :*: b) from write to read
                bcc2sbm q                       -- run q (as read stack has value of type e :*: b)
                dropFrame                       -- drop frame containing e :*: b
                bwd x                           -- bwd to beginning
bcc2sbm (Factor -- :: Mph Types a (b :*: c)
                (p :: Mph Types a b)
                (q :: Mph Types a c)) = do
        bcc2sbm p
        bcc2sbm q
bcc2sbm (Fst :: Mph Types ab a)     = do
    copy (bsize (undefined :: a))
bcc2sbm (Snd :: Mph Types ab b)     = do
    fwd (bsizf (undefined :: ab))
    copy (bsize (undefined :: b))
    bwd (bsizf (undefined :: ab))
bcc2sbm (Curry -- :: Mph o r (a :=>: b) 
            (f ::  Mph Types (r :*: a) b)) = do 
    let rSize = bsize (undefined :: r)
    r <- readMany rSize
    pushClosure (bcc2sbm f) r
bcc2sbm (Eval :: Mph Types a2bna b) = do 
    let a2bsize = bsizf (undefined :: a2bna)
        aSize   = bsizs (undefined :: a2bna)
    fwd a2bsize
    a <- readMany aSize
    (f,r) <- popClosure
    let rnaSize = length r + aSize
    newFrame rnaSize
    mapM_ write r
    mapM_ write a
    moveFrame
    f
    dropFrame
    return ()