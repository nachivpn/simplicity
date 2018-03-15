module SBM where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe (catMaybes)

-- |A bit of data is simply a boolean
type Bit = Bool

-- |A Frame is a chunk of memory with a pointer
type Frame = ([Maybe Bit],Int)

-- |A "Stack" is a stack (or list) of frames
type Stack = [Frame]

-- |A machine consists of two stacks, one for reading and the other for writing
data Machine = Machine {
    readStack   :: Stack,
    writeStack  :: Stack }
    deriving Show

-- |Instructions of the simplicity bit machine
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
    deriving Show

-- |Stateful Bit Machine (SBM) monad
type SBM = State Machine

-- |run one instruction in SBM monad
runInstr :: Instr -> SBM (Maybe Bit)
runInstr Nop             = return Nothing
runInstr (Write b)       = write b >> return Nothing
runInstr (Copy n)        = copy n >> return Nothing
runInstr (Skip n)        = skip n >> return Nothing
runInstr (Fwd n)         = fwd n >> return Nothing
runInstr (Bwd n)         = bwd n >> return Nothing
runInstr (NewFrame n)    = newFrame n >> return Nothing
runInstr MoveFrame       = moveFrame >> return Nothing
runInstr DropFrame       = dropFrame >> return Nothing
runInstr Read            = uncurry (!!) <$> fst <$> activeFrame readStack

-- |run the instructions and return the read bits
run :: [Instr] -> [Bit]
run = catMaybes . runS
    where
    runS is = evalState (mapM runInstr is) startMachine 
    startMachine = Machine {readStack = [],writeStack = []}

-- |Writes a bit to the current read pointer
write :: Bit -> SBM ()
write b = do
    ((awf,wp),tws) <- activeFrame writeStack
    case awf !! wp of
        Nothing     -> do
            let (l,r) = splitAt wp awf
                awf' = l ++ [Just b] ++ tail r
            modify (\s -> s {writeStack  = (awf',wp + 1) : tws})

-- |Copies n cells from active read frame to write frame
copy :: Int -> SBM ()
copy n = do
    ((arf,rp),trs) <- activeFrame readStack
    ((awf,wp),tws) <- activeFrame writeStack
    let cells   = (take n . drop rp) arf
        (l,r)    = splitAt wp awf
        awf' = l ++ cells ++ (drop n r)
    modify (\s -> s {writeStack  = (awf',wp + n) : tws})

-- |skips the write pointer by n
skip :: Int -> SBM ()
skip n = do
    ((awf,wp),tws) <- activeFrame writeStack
    modify (\s -> s {writeStack  = (awf,wp + n) : tws})

-- |moves the read pointer forward by n
fwd :: Int -> SBM ()
fwd n = do
    ((arf,rp),trs) <- activeFrame readStack
    modify (\s -> s {readStack  = (arf,rp + n) : trs})

-- |moves the read pointer back by n
bwd :: Int -> SBM ()
bwd n = do
    ((arf,rp),trs) <- activeFrame readStack
    modify (\s -> s {readStack  = (arf,rp - n) : trs})

-- |pushes a new frame of size n on the stack 
newFrame :: Int -> SBM ()
newFrame n = do
    let frame  = (replicate n Nothing,0)
    ws <- writeStack <$> get
    modify (\s -> s {writeStack  = frame : ws})

-- |moves active from from write stack to read stack 
moveFrame :: SBM ()
moveFrame = do
    (hws,tws) <- activeFrame writeStack
    rs' <- (hws :) <$> readStack <$> get
    modify (\s -> s {
        writeStack  = tws,
        readStack   = rs'})

-- |drops active frame from read stack 
dropFrame :: SBM ()
dropFrame = do
    trs <- tail <$> readStack <$> get
    modify (\s -> s {readStack  = trs})

-- |Returns the active frame (topmost) from a stack 
activeFrame :: 
    (Machine -> Stack) -- ^ specifies the stack to return the frame from
    -> SBM (Frame,[Frame]) -- ^ returns active frame and remaining stack
activeFrame f = (\stack -> (head stack, tail stack)) . f <$> get

