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

getVar :: String -> CodeGen Instruction
getVar name = do
    entry <- lookupSymTab name
    return $ push (getMemSeg entry) (getIndex entry)

setVar :: String -> CodeGen ()
setVar name = do
    entry <- lookupSymTab name
    addInst $ pop (getMemSeg entry) (getIndex entry)

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
emitKeywordConstant This = getVar "this" >>= addInst

emitVarName :: VarName -> CodeGen ()
emitVarName (Identifier name) = getVar name >>= addInst

emitTermOp :: UnaryOp -> Term -> CodeGen ()
emitTermOp op term = emitTerm term >> emitOp op
    where emitOp Negate = addInst neg
          emitOp BitwiseNot = addInst not

emitArrayRead :: VarName -> ArrayIdx -> CodeGen ()
emitArrayRead (Identifier name) idx = do
    emitExpression idx
    var <- getVar name
    addInsts [var, add, pop pointer 1, push that 0]

emitSubroutineCall :: SubroutineCall -> CodeGen ()
emitSubroutineCall (FreeCall (Identifier name) exprs) = do
    mapM_ emitExpression exprs
    addInst $ call name (toInteger $ length exprs)
emitSubroutineCall (ClassCall (Identifier varName) (Identifier subName) exprs) = do
    var <- getVar varName
    addInst var
    mapM_ emitExpression exprs
    addInst $ call subName (toInteger $ length exprs + 1)

emitTerm :: Term -> CodeGen ()
emitTerm (IC i)  = emitIntegerConstant i
emitTerm (SC s)  = emitStringConstant s
emitTerm (KC k)  = emitKeywordConstant k
emitTerm (VN vn) = emitVarName vn
emitTerm (TermOp op term) = emitTermOp op term
emitTerm (ParenExp exp) = emitExpression exp
emitTerm (VNArr name idx) = emitArrayRead name idx
emitTerm (SubCall subCall) = emitSubroutineCall subCall

emitExpression :: Expression -> CodeGen ()
emitExpression (Expression term termops) =
    emitTerm term >> mapM_ f termops
    where f (op, term) = emitTerm term >> emitOp op

emitStatement :: Statement -> CodeGen ()
emitStatement (LetStatement (Identifier varName) Nothing expr) = do
    emitExpression expr
    setVar varName
emitStatement (LetStatement (Identifier varName) (Just arrIdx) expr) = do
    emitExpression expr
    var <- getVar varName
    emitExpression arrIdx
    addInsts [var, add, pop pointer 1, pop that 0]
emitStatement (IfStatement cond stmts Nothing) = do
    emitExpression cond
    l@(Label label) <- genLabel "ENDIF"
    addInsts [not, ifGoto label]
    mapM_ emitStatement stmts
    addInst l
emitStatement (IfStatement cond stmts (Just elseStmts)) = do
    emitExpression cond
    l@(Label lelse) <- genLabel "ELSE"
    lend@(Label lendif) <- genLabel "ENDIF"
    addInsts [not, ifGoto lelse]
    mapM_ emitStatement stmts
    addInst $ goto lendif
    addInst l
    mapM_ emitStatement elseStmts
    addInst lend

-- TODO
emitStatement _ = undefined

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
-- test  "if (z < 10) { let x = -x[y+1] * this.func(1+ 3); }" statement emitStatement  teststate

test' s p f state = case parseAttempt of
                Left m    -> Left $ show m
                Right ast -> codegen' ast f state
    where parseAttempt = parseJackSnippet s p

testCompState :: [(String, Integer)] -> CompileState
testCompState input = initCompileState "fakeclass" $ M.fromList entries
    where f (s, i) = (s, Entry Int argument i)
          entries = map f input

teststate = testCompState [("this",0),("x",1),("y",2),("z", 3)]

test s p f state = mapM_ print $ fromRight [] $ test' s p f state