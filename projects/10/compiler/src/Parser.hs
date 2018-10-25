module Parser(myparse) where

--import Text.Parsec (parse)
--import Text.Parsec.String (Parser)
--import Text.Parsec.Char
--import Text.Parsec.Combinator
--import Text.Parsec.Token
import Control.Monad (void)
import Control.Applicative ((<*), (*>))
import Text.ParserCombinators.Parsec
import Grammar

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n\r")

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

symbol :: Char -> Parser ()
symbol c = void $ lexeme $ char c

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

keyword :: String -> Parser String
keyword k = do
    string k
    notFollowedBy alphaNum
    notFollowedBy $ char '_'
    whitespace
    return k

-- Check out: https://jakewheat.github.io/intro_to_parsing/#_comments

identifier :: Parser String
identifier = lexeme ((:) <$> firstChar <*> many nonFirstChar)
    where
        firstChar = letter <|> char '_'
        nonFirstChar = digit <|> firstChar

integer :: Parser Integer
integer = do
    i <- many1 digit
    whitespace
    return (read i)

stringLit :: Parser String
stringLit = do
    char '"'
    s <- manyTill anyChar (char '"')
    whitespace
    return s

deol :: Parser Class
deol = do
    whitespace
    keyword "class"
    className <- identifier
    symbol '{'
    -- TODO: put stuff here
    symbol '}'
    return $ Class (Identifier className) [] []

-- parse deol "(unknown)" "class stuff23 {}"

myparse :: String -> Either String Class
myparse text = Right $ Class (Identifier "myclass") [] []