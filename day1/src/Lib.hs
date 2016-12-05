module Lib
    ( someFunc
      , followInstructions
      , parseSteps
      , move
    ) where

import Data.List.Split

someFunc :: IO ()
someFunc = do
  contents <- readFile "input.txt"
  let (_, (x, y)) = followInstructions contents

  putStrLn ("answer="++ (show x) ++" + "++ (show y))
  putStrLn ("="++ (show $ abs x + abs y))

parseSteps :: String -> [(Char,Integer)]
parseSteps input = map (\f -> (head f, read (tail f) :: Integer)) $ splitOn ", " input

move ('R', howMuch) ('R', (x, y)) = ('D', (x, y - howMuch))
move ('R', howMuch) ('L', (x, y)) = ('U', (x, y + howMuch))
move ('R', howMuch) ('U', (x, y)) = ('R', (x + howMuch, y))
move ('R', howMuch) ('D', (x, y)) = ('L', (x - howMuch, y))

move ('L', howMuch) ('R', (x, y)) = ('U', (x, y + howMuch))
move ('L', howMuch) ('L', (x, y)) = ('D', (x, y - howMuch))
move ('L', howMuch) ('U', (x, y)) = ('L', (x - howMuch, y))
move ('L', howMuch) ('D', (x, y)) = ('R', (x + howMuch, y))

followInstructions :: String -> (Char, (Integer, Integer))
followInstructions inputString  =  do
  let steps = reverse $ parseSteps inputString
  foldr (\y acc -> move y acc) ('U', (0,0)) steps 
