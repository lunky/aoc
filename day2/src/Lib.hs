module Lib
    ( someFunc
      , followInstructions
      , followInstructionsStar
      , processLines
      , processLinesStar
      , moveStar
    ) where
import Data.Char
import Debug.Trace

someFunc :: IO ()
someFunc = do
  contents <- readFile "input.txt"
  let answer = processLines contents
  putStrLn $ "Answer = " ++ show answer
  let answerStar = processLinesStar contents
  putStrLn $ "Answer = " ++ show answerStar

move :: Char -> Int -> Int
move 'R' 3 = 3
move 'R' 6 = 6
move 'R' 9 = 9
move 'R' from = from + 1
move 'L' 1 = 1
move 'L' 4 = 4
move 'L' 7 = 7
move 'L' from = from - 1
move 'U' 1 = 1
move 'U' 2 = 2
move 'U' 3 = 3
move 'U' from = from - 3
move 'D' 7 = 7
move 'D' 8 = 8
move 'D' 9 = 9
move 'D' from = from + 3

moveStar :: Char -> Char -> Char 

moveStar  'U' '3' = '1'
moveStar  'U' '6' = '2'
moveStar  'U' '7' = '3'
moveStar  'U' '8' = '4'
moveStar  'U' 'A' = '6'
moveStar  'U' 'B' = '7'
moveStar  'U' 'C' = '8'
moveStar  'U' 'D' = 'B'

moveStar  'D' '1' = '3'
moveStar  'D' '2' = '6'
moveStar  'D' '3' = '7'
moveStar  'D' '4' = '8'
moveStar  'D' '6' = 'A'
moveStar  'D' '7' = 'B'
moveStar  'D' '8' = 'C'
moveStar  'D' 'B' = 'D'

moveStar  'R' '2' = '3'
moveStar  'R' '3' = '4'
moveStar  'R' '5' = '6'
moveStar  'R' '6' = '7'
moveStar  'R' '7' = '8'
moveStar  'R' '8' = '9'
moveStar  'R' 'A' = 'B'
moveStar  'R' 'B' = 'C'

moveStar  'L' '3' = '2'
moveStar  'L' '4' = '3'
moveStar  'L' '6' = '5'
moveStar  'L' '7' = '6'
moveStar  'L' '8' = '7'
moveStar  'L' '9' = '8'
moveStar  'L' 'B' = 'A'
moveStar  'L' 'C' = 'B'
-- otherwise
moveStar  _ from = from

processLinesStar :: [Char] -> [Char]
processLinesStar content = do
  let contentLines = lines content
  tail $ reverse $scanr (\l acc -> followInstructionsStar acc l) '7' (reverse contentLines)

processLines :: [Char] -> [Char]
processLines content = do
  let contentLines = lines content
  tail $ map intToDigit $ reverse $scanr (\l acc -> followInstructions acc l) 5 (reverse contentLines)

followInstructions :: Int -> [Char] -> Int
followInstructions start steps =  do 
  foldr (\l acc -> move l acc) start (reverse steps)

followInstructionsStar :: Char -> [Char] -> Char
followInstructionsStar start steps =  do 
  foldr (\l acc -> moveStar l acc) start (reverse steps)
