module CodeGen where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad (msum)
import Data.DList (fromList, toList, DList(..), singleton)
import Data.Char
import Data.List (transpose)
import Data.Either
import qualified Data.Map.Strict as M

import Grammar
import Instructions
import Parser hiding (className)

import Prelude hiding (True, False, and, or, not)

type ErrorMsg = String
type Program = [Instruction]
type SymTab = M.Map String TableEntry

data TableEntry = Entry
    { getType   :: Type
    , getMemSeg :: MemSeg
    , getIndex  :: Integer
    } deriving (Show)

data CompileState = CompileState
    { labelIndex  :: Integer
    , classSymTab :: SymTab
    , subSymTab   :: SymTab
    , className   :: String
    } deriving (Show)

type CodeGen a = ExceptT String (StateT CompileState (Writer (DList Instruction))) a

initCompileState :: String -> SymTab -> CompileState
initCompileState name symtab = CompileState
    { labelIndex  = 0
    , classSymTab = symtab
    , subSymTab   = M.empty
    , className   = name
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
mkPop = addInst $ pop argument 1

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

emitVarName :: VarName -> CodeGen ()
emitVarName (Identifier name) = do
    entry <- lookupSymTab name
    addInst $ push (getMemSeg entry) (getIndex entry)

emitTerm :: Term -> CodeGen ()
emitTerm (IC i)  = emitIntegerConstant i
emitTerm (SC s)  = emitStringConstant s
emitTerm (KC k)  = emitKeywordConstant k
emitTerm (VN vn) = emitVarName vn
-- TODO
emitTerm _ = undefined

emitExpression :: Expression -> CodeGen ()
emitExpression (Expression term termops) =
    emitTerm term >> mapM_ f termops
    where f (op, term) = emitTerm term >> emitOp op

emitOp :: Op -> CodeGen ()
emitOp Plus        = addInst add
emitOp Minus       = addInst sub
emitOp Mult        = addInst $ call "Math.multiply" 2
emitOp Divide      = addInst $ call "Math.divide" 2
emitOp BitwiseAnd  = addInst and
emitOp BitwiseOr   = addInst or
emitOp LessThan    = addInst lt
emitOp GreaterThan = addInst gt
emitOp Equals      = addInst eq

codegen' :: a -> (a -> CodeGen b) -> CompileState -> Either ErrorMsg Program
codegen' ast f state = case msg of
            Left m  -> Left m
            Right _ -> Right $ toList insts
    where
        result = runWriter $ (evalStateT $ runExceptT (f ast)) state
        (msg, insts) = result

getClassMemSeg :: ClassVarType -> MemSeg
getClassMemSeg Static = static
getClassMemSeg Field  = this

lookupSymTab :: String -> CodeGen TableEntry
lookupSymTab name = do
    state <- get
    let result = msum [M.lookup name $ subSymTab state,
                       M.lookup name $ classSymTab state]
    case result of
        Nothing -> throwError $ "Couldn't find variable: " ++ name
        Just x  -> return x

codegen :: Class -> Either ErrorMsg Program
codegen c@(Class name vars _) = codegen' c emitClass $
    initCompileState (getIdentifier name) classSymTab
    where classSymTab = M.fromList $ go coll 0 0
          f (ClassVarDec varty ty names) = map h names
            where h (Identifier i) = (i, Entry ty (getClassMemSeg varty) 0)
          coll = concatMap f vars
          go ((s, e):xs) staticNum thisNum =
            if getMemSeg e == static
            then (s, e {getIndex = staticNum}) : go xs (staticNum + 1) thisNum
            else (s, e {getIndex = thisNum}) : go xs staticNum (thisNum + 1)
          go [] _ _ = []

emitClass :: Class -> CodeGen ()
emitClass _ = undefined

-- test "123" integerConstant emitIntegerConstant initCompileState
-- test "\"HOW MANY NUMBERS? \"" stringConstant emitStringConstant initCompileState
-- test "x + 1" expression emitExpression teststate

test' s p f state = case parseAttempt of
                Left m    -> Left $ show m
                Right ast -> codegen' ast f state
    where parseAttempt = parseJackSnippet s p

testCompState :: [(String, Integer)] -> CompileState
testCompState input = initCompileState "fakeclass" $ M.fromList entries
    where f (s, i) = (s, Entry Int argument i)
          entries = map f input

teststate = testCompState [("x",0),("y",1),("z", 2)]

test s p f state = mapM_ print $ fromRight [] $ test' s p f state