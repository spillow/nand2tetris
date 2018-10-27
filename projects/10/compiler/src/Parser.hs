module Parser(myparse) where

--import Text.Parsec (parse)
import Control.Monad (void)
import Control.Applicative ((<*), (*>))
import Text.ParserCombinators.Parsec
import Grammar

import Prelude hiding (True, False)

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

symbol :: Char -> Parser Symbol
symbol c = Symbol <$> lexeme (char c)

keyword :: String -> Parser Keyword
keyword k = do
    string k
    notFollowedBy alphaNum
    notFollowedBy $ char '_'
    whitespace
    return $ Keyword k

-- Check out: https://jakewheat.github.io/intro_to_parsing/#_comments

whitespace :: Parser ()
whitespace =
    choice [simpleWhitespace *> whitespace
           ,lineComment *> whitespace
           ,blockComment *> whitespace
           ,return ()]
  where
    lineComment = try (string "//")
                  *> manyTill anyChar (void eol <|> eof)
    blockComment = try (string "/*")
                   *> manyTill anyChar (try $ string "*/")
    simpleWhitespace = void $ many1 (oneOf " \r\t\n")

identifier :: Parser Identifier
identifier = Identifier <$> lexeme ((:) <$> firstChar <*> many nonFirstChar)
    where firstChar = letter <|> char '_'
          nonFirstChar = digit <|> firstChar

varName = identifier
subroutineName = identifier
className = identifier

integerConstant :: Parser IntegerConstant
integerConstant = do
    i <- many1 digit
    whitespace
    return $ IntegerConstant (read i)

stringConstant :: Parser StringConstant
stringConstant = do
    char '"'
    s <- manyTill anyChar (char '"')
    whitespace
    return $ StringConstant s

op :: Parser Op
op = choice [plus, minus, star, div, bitand, bitor, lt, gt, eq]
    where conv x y = const x <$> symbol y
          plus   = conv Plus '+'
          minus  = conv Minus '-'
          star   = conv Mult '*'
          div    = conv Divide '/'
          bitand = conv BitwiseAnd '&'
          bitor  = conv BitwiseOr '|'
          lt     = conv LessThan '<'
          gt     = conv GreaterThan '>'
          eq     = conv Equals '='

unaryOp :: Parser UnaryOp
unaryOp = choice [neg, not]
    where conv x y = const x <$> symbol y
          neg = conv Negate '-'
          not = conv BitwiseNot '~'
        
keywordConstant :: Parser KeywordConstant
keywordConstant = choice [p "true" True, p "false" False,
                          p "null" Null, p "this" This]
    where p x y = try (const y <$> keyword x)

expressionList :: Parser [Expression]
expressionList = expression `sepBy` symbol ','

subroutineCall :: Parser SubroutineCall
subroutineCall = try freeCall <|> try subCall
    where freeCall = do
            name <- subroutineName
            exprList <- parenExprList
            return $ FreeCall name exprList
          subCall = do
            name <- varName
            symbol '.'
            subName <- subroutineName
            exprList <- parenExprList
            return $ ClassCall name subName exprList

term :: Parser Term
term = choice [m integerConstant IC, m stringConstant SC,
               m keywordConstant KC, try arrayIdx,
               VN <$> try varName, SubCall <$> try subroutineCall,
               try parExpr, try termOp]
    where m x y = y <$> try x
          arrayIdx = do
            name <- varName
            expr <- bracketExpr
            return $ VNArr name expr
          parExpr = ParenExp <$> parenExpr
          termOp = do
            i <- unaryOp
            j <- term
            return $ TermOp i j
        

expression :: Parser Expression
expression = do
    i <- term
    j <- many opTerm
    return $ Expression i j
    where opTerm = do
            o <- op
            t <- term
            return (o, t)

statements :: Parser Statements
statements = many statement

statement :: Parser Statement
statement = choice [letStatement, ifStatement,
                    whileStatement, doStatement, returnStatement]

delimParser :: Char -> Parser a -> Char -> Parser a
delimParser c1 p c2 = do
    symbol c1
    contents <- p
    symbol c2
    return contents

bracketExpr    = delimParser '[' expression ']'
bracedStmts    = delimParser '{' statements '}'
parenExpr      = delimParser '(' expression ')'
parenExprList  = delimParser '(' expressionList ')'
parenParamList = delimParser '(' parameterList ')'

ifStatement :: Parser Statement
ifStatement = do
    try $ keyword "if"
    cond <- parenExpr
    stmts <- bracedStmts
    elif <- optionMaybe $ keyword "else" *> bracedStmts
    return $ IfStatement cond stmts elif

whileStatement :: Parser Statement
whileStatement = do
    try $ keyword "while"
    cond <- parenExpr
    stmts <- bracedStmts
    return $ WhileStatement cond stmts

doStatement :: Parser Statement
doStatement = do
    try $ keyword "do"
    call <- subroutineCall
    symbol ';'
    return $ DoStatement call

returnStatement :: Parser Statement
returnStatement = do
    try $ keyword "return"
    expr <- optionMaybe expression
    symbol ';'
    return $ ReturnStatement expr

letStatement :: Parser Statement
letStatement = do
    try $ keyword "let"
    name <- varName
    arrIdx <- optionMaybe bracketExpr
    symbol '='
    expr <- expression
    symbol ';'
    return $ LetStatement name arrIdx expr

typeParse :: Parser Type
typeParse = choice [const Int <$> try (keyword "int")
                  , const Char <$> try (keyword "char")
                  , const Boolean <$> try (keyword "boolean")
                  , TypeName <$> try className]

parseTyDecls :: Parser (Type, [VarName])
parseTyDecls = do
    ty <- typeParse
    v1 <- varName
    vars <- varName `sepBy` symbol ','
    symbol ';'
    return (ty, v1:vars)

varDec :: Parser VarDec
varDec = do
    try $ keyword "var"
    (ty, vars) <- parseTyDecls
    return $ VarDec ty vars

parameterList :: Parser ParameterList
parameterList = ParameterList <$> p `sepBy` symbol ','
    where p = do
            ty <- typeParse
            v  <- varName
            return (ty, v)
        
subroutineBody :: Parser SubroutineBody
subroutineBody = do
    symbol '{'
    vars  <- many varDec
    stmts <- statements
    symbol '}'
    return $ SubroutineBody vars stmts

subroutineDec :: Parser SubroutineDec
subroutineDec = do
    subVariety <- decTy
    subType    <- subTy
    subName    <- subroutineName
    params     <- parenParamList
    subBody    <- subroutineBody
    return $ SubroutineDec subVariety subType subName params subBody
    where decTy = choice [const Constructor <$> try (keyword "constructor")
                        , const Function <$> try (keyword "function")
                        , const Method <$> try (keyword "method")]
          subTy = choice [const Void <$> try (keyword "void")
                        , SubTy <$> typeParse]

classVarDec :: Parser ClassVarDec
classVarDec = do
    classVarTy <- varTy
    (ty, vars) <- parseTyDecls
    return $ ClassVarDec classVarTy ty vars
    where varTy = choice [const Static <$> try (keyword "static")
                        , const Field <$> try (keyword "field")]

parseClass :: Parser Class
parseClass = do
    whitespace
    keyword "class"
    name <- className
    symbol '{'
    varDecs <- many classVarDec
    subDecs <- many subroutineDec
    symbol '}'
    return $ Class name varDecs subDecs

-- parse subroutineCall   "(unknown)" "var . launch(sum + a[i+2], (5+3)*4  , -8 /* some good comments here */, \"filename\" ) // good func"
-- parse parseClass "(unknown)" "class MyClass { function stuff(int pp) { while ( a < 3 ) { let letter = 3*4; } if (2+2 = 4) { if(1){} }  else { let stuffing = v[3];  } } }"

myparse :: String -> Either String Class
myparse text = Right $ Class (Identifier "myclass") [] []