module Day9Lib
    (
     working
     ,decompress
     ,decompress2
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
  comp <- count howMany anyChar
  return $ concat $ replicate repeatX comp

compressed2 :: Parser String
compressed2 = do 
  void $  char '('
  howMany <- num
  void $ char 'x'
  repeatX <- num
  void $  char ')'
  comp <- (count howMany anyChar)
  let rest = concat $ take repeatX $ repeat comp
  return $ decompress2 rest
  

num :: Parser Int
num = do
    n <- many1 digit
    return (read n)

notCompressed :: Parser Char 
notCompressed =  satisfy (/='(')

decompress2 :: String -> String
decompress2 str = do
  let response = regularParse (many1 (compressed2<|>(many1 notCompressed))) str
  case response of 
    Left msg -> error $ show msg
    Right answer ->  concat answer

decompress :: String -> String
decompress str = do
  let response = regularParse (many1 (compressed<|> (many1 notCompressed))) str
  case response of 
    Left msg -> error $ show msg
    Right answer ->  concat answer

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""
