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

getVar :: String -> CodeGen Instruction
getVar name = do
    entry <- lookupSymTab name
    return $ push (getMemSeg entry) (getIndex entry)

unionSubSymTab :: SymTab -> CodeGen ()
unionSubSymTab otherTab = do
    state <- get
    let currTab = subSymTab state
    put $ state { subSymTab = M.union currTab otherTab }

flushSubSymTab :: CodeGen ()
flushSubSymTab = do
    s <- get
    put $ s {subSymTab = M.empty}

maybeGetVar :: String -> CodeGen (Maybe (Instruction, Type))
maybeGetVar name = do
    entry <- maybeLookupSymTab name
    case entry of
        Nothing -> return Nothing
        Just x  -> return $ Just (push (getMemSeg x) (getIndex x), getType x)

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

getClassName :: CodeGen String
getClassName = do
    state <- get
    return $ className state

emitSubroutineCall :: SubroutineCall -> CodeGen ()
emitSubroutineCall (FreeCall (Identifier fnname) exprs) = do
    thisPtr <- getVar "this"
    addInst thisPtr
    name <- getClassName
    mapM_ emitExpression exprs
    addInst $ call (name ++ "." ++ fnname) (toInteger $ length exprs + 1)
emitSubroutineCall (ClassCall (Identifier varName) (Identifier subName) exprs) = do
    var <- maybeGetVar varName
    case var of
        Just (v, TypeName (Identifier ty)) -> methodCall v ty
        Just _                             -> throwError "var type is not a class!"
        Nothing                            -> staticCall
    where methodCall v ty = do
            addInst v
            mapM_ emitExpression exprs
            addInst $ call (ty ++ "." ++ subName) (toInteger $ length exprs + 1)
          staticCall = do
            mapM_ emitExpression exprs
            addInst $ call (varName ++ "." ++ subName) (toInteger $ length exprs)

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

subPreamble start extraLocals subName body params = do
    flushSubSymTab
    cname <- getClassName
    addInst $ function (cname ++ "." ++ subName) (getNumLocalVars body + extraLocals)
    addParamList start params

emitSubroutineDec :: SubroutineDec -> CodeGen ()
emitSubroutineDec (SubroutineDec Constructor (SubTy ty)
                   (Identifier subName) params body) = do
    subPreamble 0 1 subName body params
    numFieldVars <- getNumFieldVars
    addInst $ push constant numFieldVars
    addInst $ call "Memory.alloc" 1
    addInsts [pop local 0, push local 0, pop pointer 0]
    addSubSymTabEntry "this" $ Entry ty local 0
    emitSubroutineBody 1 body
emitSubroutineDec (SubroutineDec Function _
                   (Identifier subName) params body) = do
    subPreamble 0 0 subName body params
    emitSubroutineBody 0 body
emitSubroutineDec (SubroutineDec Method _
                   (Identifier subName) params body) = do
    subPreamble 1 0 subName body params
    cname <- getClassName
    addSubSymTabEntry "this" $ Entry (TypeName $ Identifier cname) argument 0
    addInsts [push argument 0, pop pointer 0]
    emitSubroutineBody 0 body
emitSubroutineDec _ = throwError "constructor must return a type!"

mapS :: (s -> a -> (s, b)) -> s -> [a] -> [b]
mapS f s (x:xs) = res : mapS f news xs
    where (news, res) = f s x
mapS _ _ [] = []

addSubSymTabEntry :: String -> TableEntry -> CodeGen ()
addSubSymTabEntry k e = do
    s <- get
    let tab = subSymTab s
    put $ s { subSymTab = M.insert k e tab }

getNumFieldVars :: CodeGen Integer
getNumFieldVars = do
    s <- get
    let symtab = classSymTab s
    return $ toInteger $ length [seg | (Entry _ seg _) <- M.elems symtab, seg == this]

getNumLocalVars :: SubroutineBody -> Integer
getNumLocalVars (SubroutineBody vars _) = sum $ map f vars
    where f (VarDec _ names) = toInteger $ length names

emitSubroutineBody :: Integer -> SubroutineBody -> CodeGen ()
emitSubroutineBody start (SubroutineBody vars stmts) = do
    unionSubSymTab localMap
    mapM_ emitStatement stmts
    where entries = [(name, Entry {getType = ty, getMemSeg = local, getIndex = 0}) |
                     (VarDec ty names) <- vars, (Identifier name) <- names]
          f i (s, val) = (i+1, (s, val {getIndex = i}))
          localMap = M.fromList $ mapS f start entries

addParamList :: Integer -> ParameterList -> CodeGen ()
addParamList start (ParameterList params) = do
    state <- get
    let paramSyms = M.fromList $ zipWith f params [start..]
    put $ state {subSymTab = paramSyms}
    where f (ty, Identifier name) i =
            (name, Entry {getType = ty, getMemSeg = argument, getIndex = i})

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
emitStatement (WhileStatement cond stmts) = do
    toplabel@(Label ltoplabel) <- genLabel "TOP_LABEL"
    bottomlabel@(Label lbottomlabel) <- genLabel "BOTTOM_LABEL"
    addInst toplabel
    emitExpression cond
    addInsts [not, ifGoto lbottomlabel]
    mapM_ emitStatement stmts
    addInst $ goto ltoplabel
    addInst bottomlabel
emitStatement (DoStatement subCall) = do
    emitSubroutineCall subCall
    addInst $ pop temp 0
emitStatement (ReturnStatement (Just val)) = do
    emitExpression val
    addInst return'
emitStatement (ReturnStatement Nothing) = do
    addInst $ push constant 0
    addInst return'

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
    result <- maybeLookupSymTab name
    case result of
        Nothing -> throwError $ "Couldn't find variable: " ++ name
        Just x  -> return x

maybeLookupSymTab :: String -> CodeGen (Maybe TableEntry)
maybeLookupSymTab name = do
    state <- get
    return $ msum [M.lookup name $ subSymTab state,
                   M.lookup name $ classSymTab state]

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
emitClass (Class _ _ subDecs) = mapM_ emitSubroutineDec subDecs

-- test "123" integerConstant emitIntegerConstant initCompileState
-- test "\"HOW MANY NUMBERS? \"" stringConstant emitStringConstant initCompileState
-- test "x + 1" expression emitExpression teststate
-- test  "if (z < 10) { let x = -x[y+1] * func(1+3); }" statement emitStatement  teststate
-- test  "{var int t,g,b; let g = 0;}" subroutineBodyemitSubroutineBody teststate

test' s p f state = case parseAttempt of
                Left m    -> Left $ show m
                Right ast -> codegen' ast f state
    where parseAttempt = parseJackSnippet s p

testCompState :: [(String, Integer, Type)] -> CompileState
testCompState input = initCompileState "fakeclass" $ M.fromList entries
    where f (s, i, ty) = (s, Entry ty argument i)
          entries = map f input

teststate = testCompState [("this",0,classTy),("x",1,Int),("y",2,Int),("z", 3,Int),("w",4,classTy)]
    where classTy = TypeName $ Identifier "fakeclass"

test s p f state = mapM_ print $ fromRight [] $ test' s p f state