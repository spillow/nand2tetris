module CodeGen where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.DList (fromList, toList, DList(..), singleton)
import Data.Char
import Data.List
import Data.Either

import Grammar
import Instructions
import Parser

import Prelude hiding (True, False, and, or, not)

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

genLabel :: String -> CodeGen Instruction
genLabel prefix = do
    s <- get
    let idx = labelIndex s
    put s { labelIndex = idx + 1}
    return $ label (prefix ++ show idx)

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
mkLabel = genLabel "IF_TRUE" >>= addInst

doAll :: CodeGen ()
doAll = mkPush >> mkSomething >> mkPop >> mkLabel

emitIntegerConstant :: IntegerConstant -> CodeGen ()
emitIntegerConstant i = addInst $ push constant (getIntegerConstant i)

emitStringConstant :: StringConstant -> CodeGen ()
emitStringConstant sc = addInsts $
        push constant (toInteger len) :
        call "String.new" 1 :
        interleave pushs appendChars
    where s = getStringConstant sc
          len = length s
          pushs = map (push constant . toInteger . ord) s
          appendChars = replicate len $ call "String.appendChar" 2
          interleave xs ys = concat (transpose [xs, ys])

emitKeywordConstant :: KeywordConstant -> CodeGen ()
emitKeywordConstant True = addInsts [push constant 0, not]
emitKeywordConstant False = addInst $ push constant 0
emitKeywordConstant Null = addInst $ push constant 0
-- TODO
emitKeywordConstant This = undefined

codegen' :: a -> (a -> CodeGen b) -> CompileState -> Either ErrorMsg Program
codegen' ast f state = case msg of
            Left m  -> Left m
            Right _ -> Right $ toList insts
    where
        result = runWriter $ (evalStateT $ runExceptT (f ast)) state
        (msg, insts) = result

codegen :: Class -> Either ErrorMsg Program
codegen c = codegen' c emitClass initCompileState

emitClass :: Class -> CodeGen ()
emitClass _ = undefined

-- test "123" integerConstant emitIntegerConstant initCompileState
-- test "\"HOW MANY NUMBERS? \"" stringConstant emitStringConstant initCompileState

test' s p f state = case parseAttempt of
                Left m    -> Left $ show m
                Right ast -> codegen' ast f state
    where parseAttempt = parseJackSnippet s p

test s p f state = mapM_ print $ fromRight [] $ test' s p f state