module SBM where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

type Bit = Bool

type Frame = [Maybe Bit]

data SBM = SBM {
    readPtr     :: Int,
    readStack   :: [Frame],
    writePtr    :: Int,
    writeStack  :: [Frame]}
    deriving Show

data Instr = 
    Nop
    | Write Bit
    | Copy Int
    | Skip Int
    | Fwd Int
    | Bwd Int
    | NewFrame Int
    | MoveFrame 
    | DropFrame
    | Read

type ESBM = ExceptT String (State SBM)

run :: Instr -> ESBM (Maybe Bit)
run Nop         = return Nothing
run (Write b)   = write b >> return Nothing

write :: Bit -> ESBM ()
write b = do
    (awf,tws) <- activeFrame writeStack
    wp <- writePtr <$> get
    if wp >= length awf
        then throwError "Index too large"
        else case awf !! wp of
            Just _      -> throwError "Value must be Nothing"
            Nothing     -> do
                let (l,r) = splitAt wp awf
                    af' = l ++ [Just b] ++ tail r
                modify (\s -> s{
                    writeStack  = af' : tws,
                    writePtr    = wp + 1})

activeFrame :: (SBM -> [Frame]) -> ESBM (Frame,[Frame])
activeFrame f = do
    rs <- f <$> get
    if length rs > 0 
        then return (head rs, tail rs)
        else throwError "Empty stack"




