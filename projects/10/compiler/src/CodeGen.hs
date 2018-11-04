module CodeGen where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.DList

--import Grammar
import Instructions

type ErrorMsg = String
type Program = [Instruction]

type CodeGen a = ExceptT String (StateT CompileState (Writer (DList Instruction))) a

initCompileState :: CompileState
initCompileState = CompileState
    { labelIndex = 0

    }

mkPush :: CodeGen ()
mkPush = tell $ singleton (push local 0)

mkPop :: CodeGen ()
mkPop = tell $ singleton (pop arg 1)

mkFail:: CodeGen ()
mkFail = throwError "Couldn't compile!"

mkSomething :: CodeGen ()
mkSomething = do
    put (CompileState 6)
    s <- get
    tell $ singleton (push pointer $ labelIndex s)

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
        result = runWriter $ (evalStateT $ runExceptT doAll) initCompileState
        (msg, insts) = result
