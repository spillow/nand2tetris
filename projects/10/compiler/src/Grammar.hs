module Grammar
    ( ClassName
    , VarName
    , SubroutineName
    , Statements
    , Cond
    , ArrayIdx
    , Keyword(..)
    , Symbol(..)
    , IntegerConstant(..)
    , StringConstant(..)
    , Identifier(..)
    , ClassVarType(..)
    , SubroutineVariety(..)
    , SubroutineType(..)
    , Class(..)
    , ClassVarDec(..)
    , Type(..)
    , SubroutineDec(..)
    , ParameterList(..)
    , SubroutineBody(..)
    , VarDec(..)
    , Statement(..)
    , Expression(..)
    , Term(..)
    , SubroutineCall(..)
    , Op(..)
    , UnaryOp(..)
    , KeywordConstant(..)
    )
where

type ClassName = Identifier
type VarName = Identifier
type SubroutineName = Identifier
type Statements = [Statement]
type Cond = Expression
type ArrayIdx = Expression

-- Lexical Elements
newtype Keyword = Keyword { getKeyword :: String } deriving (Show, Eq)
newtype Symbol = Symbol { getSymbol  :: Char } deriving (Show, Eq)
newtype IntegerConstant = IntegerConstant { getIntegerConstant :: Integer } deriving (Show, Eq)
newtype StringConstant = StringConstant { getStringConstant :: String } deriving (Show, Eq)
newtype Identifier = Identifier { getIdentifier :: String } deriving (Show, Eq)

-- Program Structure
data ClassVarType = Static | Field deriving (Show, Eq)
data SubroutineVariety = Constructor | Function | Method deriving (Show, Eq)
data SubroutineType = Void | SubTy Type deriving (Show, Eq)

data Class = Class ClassName [ClassVarDec] [SubroutineDec] deriving (Show, Eq)
data ClassVarDec = ClassVarDec ClassVarType Type [VarName] deriving (Show, Eq)
data Type = Int | Char | Boolean | TypeName ClassName deriving (Show, Eq)
data SubroutineDec = SubroutineDec SubroutineVariety SubroutineType
                     SubroutineName ParameterList SubroutineBody deriving (Show, Eq)
newtype ParameterList = ParameterList { getParamList :: [(Type, VarName)] } deriving (Show, Eq)
data SubroutineBody = SubroutineBody [VarDec] Statements deriving (Show, Eq)
data VarDec = VarDec Type [VarName] deriving (Show, Eq)

-- Statements
data Statement = LetStatement VarName (Maybe ArrayIdx) Expression
               | IfStatement Cond Statements (Maybe Statements)
               | WhileStatement Cond Statements
               | DoStatement SubroutineCall
               | ReturnStatement (Maybe Expression) deriving (Show, Eq)

-- Expressions
data Expression = Expression Term [(Op, Term)] deriving (Show, Eq)
data Term = IC IntegerConstant | SC StringConstant | KC KeywordConstant | VN VarName |
            VNArr VarName ArrayIdx | SubCall SubroutineCall | ParenExp Expression |
            TermOp UnaryOp Term deriving (Show, Eq)
data SubroutineCall = FreeCall SubroutineName [Expression]
                    | ClassCall VarName SubroutineName [Expression] deriving (Show, Eq)
data Op = Plus | Minus | Mult | Divide | BitwiseAnd | BitwiseOr | LessThan | GreaterThan | Equals
          deriving (Show, Eq)
data UnaryOp = Negate | BitwiseNot deriving (Show, Eq)
data KeywordConstant = True | False | Null | This deriving (Show, Eq)