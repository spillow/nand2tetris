module Parser where

import Control.Monad (void, liftM2)
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
    where freeCall = liftM2 FreeCall subroutineName parenExprList
          subCall = do
            name <- varName
            symbol '.'
            subName <- subroutineName
            ClassCall name subName <$> parenExprList

term :: Parser Term
term = choice [SubCall <$> try subroutineCall,
               m integerConstant IC, m stringConstant SC,
               m keywordConstant KC, try arrayIdx,
               VN <$> try varName,
               try parExpr, try termOp]
    where m x y = y <$> try x
          arrayIdx = liftM2 VNArr varName bracketExpr
          parExpr = ParenExp <$> parenExpr
          termOp = liftM2 TermOp unaryOp term

expression :: Parser Expression
expression = liftM2 Expression term (many opTerm)
    where opTerm = liftM2 (,) op term

statements :: Parser Statements
statements = many statement

statement :: Parser Statement
statement = choice [letStatement, ifStatement,
                    whileStatement, doStatement, returnStatement]

delimParser :: Char -> Parser a -> Char -> Parser a
delimParser c1 p c2 = symbol c1 *> p <* symbol c2

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
    WhileStatement cond <$> bracedStmts

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
    vars <- many $ symbol ',' *> varName
    symbol ';'
    return (ty, v1:vars)

varDec :: Parser VarDec
varDec = do
    try $ keyword "var"
    (ty, vars) <- parseTyDecls
    return $ VarDec ty vars

parameterList :: Parser ParameterList
parameterList = ParameterList <$> p `sepBy` symbol ','
    where p = liftM2 (,) typeParse varName
        
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
    SubroutineDec subVariety subType subName params <$> subroutineBody
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

parseJackSnippet :: String -> Parser a -> Either ParseError a
parseJackSnippet s p = parse p "(unknown)" s

parseJack :: FilePath -> String -> Either ParseError Class
parseJack = parse parseClass