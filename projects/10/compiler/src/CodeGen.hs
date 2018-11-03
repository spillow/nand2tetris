module CodeGen where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.DList

--import Grammar

type ErrorMsg = String
type Program = [Instruction]
type Idx = Integer
data MemSeg = Local | Constant | Temp | Pointer | This | That | Arg | Static deriving (Show)
data Instruction = Push MemSeg Idx | Pop MemSeg Idx deriving (Show)
newtype CompileState = CompileState { getIndex :: Integer } deriving (Show)

type CodeGen a = ExceptT String (StateT CompileState (Writer (DList Instruction))) a

mkPush :: CodeGen ()
mkPush = tell $ singleton (Push Local 0)

mkPop :: CodeGen ()
mkPop = tell $ singleton (Pop Arg 1)

mkFail:: CodeGen ()
mkFail = throwError "Couldn't compile!"

mkSomething :: CodeGen ()
mkSomething = do
    put (CompileState 6)
    s <- get
    tell $ singleton (Push Pointer $ getIndex s)

doAll :: CodeGen ()
--doAll = mkPush >> mkFail >> mkPop
doAll = mkPush >> mkSomething >> mkPop

--codegen :: Class -> Program
codegen :: Either ErrorMsg Program
codegen = case msg of
            Left m  -> Left m
            Right _ -> Right $ toList insts
    where
        result :: (Either String (), DList Instruction)
        result = runWriter $ (evalStateT $ runExceptT doAll) (CompileState 5)
        (msg, insts) = result
