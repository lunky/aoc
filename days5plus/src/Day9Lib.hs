module Day9Lib
    (
     working
     ,decompress
    ) where
import AocCommon
import Data.List
import Debug.Trace
import Data.Char
import Text.Parsec
import Text.Parsec.String 
import Text.Parsec.Token (symbol)
import Control.Monad (void)


working = "working"

compressed :: Parser String
compressed = do 
  void $  char '('
  howMany <- num
  void $ char 'x'
  repeatX <- num
  void $  char ')'
  comp <- (count howMany anyChar)
  return $ concat $ take repeatX $ repeat comp


num :: Parser Int
num = do
    n <- many1 digit
    return (read n)

decompress :: String -> String
decompress str = do
  let response = regularParse (many1 (compressed<|> (many1 (satisfy (/='('))))) str
  case response of 
    Left msg -> error $ show msg
    Right answer ->  concat answer

parensE :: Parser String
parensE = do
    void $  char '('
    e <- ( many ( satisfy (/= ')') ))
    void $  char ')'
    return e

parensD :: Parser String
parensD = do
    first <- ( many ( satisfy (/= '(') ))
    void $  many ( char '(')
    e <- ( many ( satisfy (/= ')') ))
    void $  many ( char ')')
    last <- many anyChar
    return $ first ++ e ++ last

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

data Parentheses = Parentheses String
                   deriving (Eq)


whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  whitespace
  return x

parens :: Parser Parentheses
parens = do
    void $ lexeme $ char '('
    e <- many1 anyChar
    void $ lexeme $ char ')'
    return (Parentheses (read e))

