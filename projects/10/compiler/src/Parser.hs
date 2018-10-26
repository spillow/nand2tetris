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
    where
        firstChar = letter <|> char '_'
        nonFirstChar = digit <|> firstChar

varName = identifier
subroutineName = identifier

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
    where
        conv x y = const x <$> symbol y
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
    where
        conv x y = const x <$> symbol y
        neg = conv Negate '-'
        not = conv BitwiseNot '~'
        
keywordConstant :: Parser KeywordConstant
keywordConstant = choice [p "true" True, p "false" False, p "null" Null, p "this" This]
    where p x y = try (const y <$> keyword x)

expressionList :: Parser [Expression]
expressionList = expression `sepBy` symbol ','

subroutineCall :: Parser SubroutineCall
subroutineCall = try freeCall <|> try subCall
    where
        freeCall = do
            name <- subroutineName
            symbol '('
            exprList <- expressionList
            symbol ')'
            return $ FreeCall name exprList
        subCall = do
            name <- varName
            symbol '.'
            subName <- subroutineName
            symbol '('
            exprList <- expressionList
            symbol ')'
            return $ ClassCall name subName exprList

term :: Parser Term
term = choice [m integerConstant IC, m stringConstant SC,
               m keywordConstant KC, try arrayIdx,
               VN <$> try varName, SubCall <$> try subroutineCall,
               try parExpr, try termOp]
    where
        m x y = y <$> try x
        arrayIdx = do
            name <- varName
            symbol '['
            expr <- expression
            symbol ']'
            return $ VNArr name expr
        parExpr = do
            symbol '('
            expr <- expression
            symbol ')'
            return $ ParenExp expr
        termOp = do
            i <- unaryOp
            j <- term
            return $ TermOp i j

expression :: Parser Expression
expression = do
    i <- term
    j <- many opTerm
    return $ Expression i j
    where
        opTerm = do
            o <- op
            t <- term
            return (o, t)

deol :: Parser Class
deol = do
    whitespace
    keyword "class"
    className <- identifier
    symbol '{'
    -- TODO: put stuff here
    symbol '}'
    return $ Class className [] []

-- parse deol "(unknown)" "class stuff23 {}"
-- parse subroutineCall   "(unknown)" "var . launch(sum + a[i+2], (5+3)*4  , -8 /* some good comments here */, \"filename\" ) // good func"

myparse :: String -> Either String Class
myparse text = Right $ Class (Identifier "myclass") [] []