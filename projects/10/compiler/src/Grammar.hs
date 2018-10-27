module Grammar where

type ClassName = Identifier
type VarName = Identifier
type SubroutineName = Identifier
type Statements = [Statement]
type Cond = Expression
type ArrayIdx = Expression

-- Lexical Elements
newtype Keyword = Keyword { getKeyword :: String } deriving (Show)
newtype Symbol = Symbol { getSymbol  :: Char } deriving (Show)
newtype IntegerConstant = IntegerConstant { getIntegerConstant :: Integer } deriving (Show)
newtype StringConstant = StringConstant { getStringConstant :: String } deriving (Show)
newtype Identifier = Identifier { getIdentifier :: String } deriving (Show)

-- Program Structure
data ClassVarType = Static | Field deriving (Show)
data SubroutineVariety = Constructor | Function | Method deriving (Show)
data SubroutineType = Void | SubTy Type deriving (Show)

data Class = Class ClassName [ClassVarDec] [SubroutineDec] deriving (Show)
data ClassVarDec = ClassVarDec ClassVarType Type [VarName] deriving (Show)
data Type = Int | Char | Boolean | TypeName ClassName deriving (Show)
data SubroutineDec = SubroutineDec SubroutineVariety SubroutineType SubroutineName ParameterList SubroutineBody deriving (Show)
newtype ParameterList = ParameterList { getParamList :: [(Type, VarName)] } deriving (Show)
data SubroutineBody = SubroutineBody [VarDec] Statements deriving (Show)
data VarDec = VarDec Type [VarName] deriving (Show)

-- Statements
data Statement = LetStatement VarName (Maybe ArrayIdx) Expression
               | IfStatement Cond Statements (Maybe Statements)
               | WhileStatement Cond Statements
               | DoStatement SubroutineCall
               | ReturnStatement (Maybe Expression) deriving (Show)

-- Expressions
data Expression = Expression Term [(Op, Term)] deriving (Show)
data Term = IC IntegerConstant | SC StringConstant | KC KeywordConstant | VN VarName | VNArr VarName ArrayIdx |
            SubCall SubroutineCall | ParenExp Expression | TermOp UnaryOp Term deriving (Show)
data SubroutineCall = FreeCall SubroutineName [Expression]
                    | ClassCall VarName SubroutineName [Expression] deriving (Show)
data Op = Plus | Minus | Mult | Divide | BitwiseAnd | BitwiseOr | LessThan | GreaterThan | Equals deriving (Show)
data UnaryOp = Negate | BitwiseNot deriving (Show)
data KeywordConstant = True | False | Null | This deriving (Show)