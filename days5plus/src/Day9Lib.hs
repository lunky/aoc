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

notCompressed :: Parser Char 
notCompressed =  satisfy (/='(')

decompress :: String -> String
decompress str = do
  let response = regularParse (many1 (compressed<|> (many1 notCompressed))) str
  case response of 
    Left msg -> error $ show msg
    Right answer ->  concat answer

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""
