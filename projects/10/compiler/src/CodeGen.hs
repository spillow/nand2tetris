module CodeGen where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.DList

--import Grammar
import Instructions

type ErrorMsg = String
type Program = [Instruction]

newtype CompileState = CompileState { labelIndex :: Integer } deriving (Show)

type CodeGen a = ExceptT String (StateT CompileState (Writer (DList Instruction))) a

initCompileState :: CompileState
initCompileState = CompileState
    { labelIndex = 0

    }

addInst :: Instruction -> CodeGen ()
addInst i = tell $ singleton i

addInsts :: [Instruction] -> CodeGen ()
addInsts xs = tell $ fromList xs

genLabel :: String -> CodeGen String
genLabel prefix = do
    s <- get
    let idx = labelIndex s
    put s { labelIndex = idx + 1}
    return $ prefix ++ show idx

mkPush :: CodeGen ()
mkPush = addInst $ push local 0

mkPop :: CodeGen ()
mkPop = addInst $ pop arg 1

mkFail:: CodeGen ()
mkFail = throwError "Couldn't compile!"

mkSomething :: CodeGen ()
mkSomething = do
    genLabel "WHILE"
    addInst $ push pointer 0

mkLabel :: CodeGen ()
mkLabel = do
    l <- genLabel "IF_TRUE"
    addInst $ label l

doAll :: CodeGen ()
doAll = mkPush >> mkSomething >> mkPop >> mkLabel

--codegen :: Class -> Program
codegen :: Either ErrorMsg Program
codegen = case msg of
            Left m  -> Left m
            Right _ -> Right $ toList insts
    where
        result :: (Either String (), DList Instruction)
        result = runWriter $ (evalStateT $ runExceptT doAll) initCompileState
        (msg, insts) = result
