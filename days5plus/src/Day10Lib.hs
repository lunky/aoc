module Day10Lib
    (
     working
     ,parseInstruction
     ,Inst(..)
     ,Target(..)
    )where
import AocCommon
import Text.Parsec
import Text.Parsec.String 
import Text.ParserCombinators.Parsec
import Control.Monad (void)

working = "working"

parseInstruction :: String -> Inst 
parseInstruction str = do
  let response = (regularParse parseBotInstruction str)
  case response of 
    Left msg -> error $ show msg
    Right answer -> answer


parseBotInstruction :: Parser Inst
parseBotInstruction = do
  void $ string "bot "  
  botNum <- num
  void $ string " gives low to "  
  lowTarget <- string "bot" <|> string "output"
  void $ char ' '
  lowValue <- num
  void $ string " and high to "  
  highTarget <- string "bot" <|> string "output"
  void $ char ' '
  highValue <- num
  
  let output = Inst botNum (getTarget lowTarget) lowValue  (getTarget highTarget) highValue
  return output

getTarget :: String -> Target
getTarget "bot" = Bot
getTarget "output" = Output

num :: Parser Integer
num = do
    n <- many1 digit
    return (read n)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

   

data Target = Bot | Output
  deriving( Eq, Show, Read )

data Inst = Inst {    botNum     :: Integer
                    , lowTarget  :: Target
                    , lowValue   :: Integer
                    , highTarget :: Target
                    , highValue  :: Integer
} deriving (Show, Eq)
