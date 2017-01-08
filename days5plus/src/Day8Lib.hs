module Day8Lib
    (
     working
     ,getRect
     ,rotate
    ) where
import Data.List
import Data.List.Split

working = "working"

parseCmd :: String -> [Int]
parseCmd = map read . splitOn "x" 

applyRowMask :: String -> String -> String
applyRowMask gridRow maskRow = 
  map (\(gridChar,maskChar) -> 
    if gridChar=='#' || maskChar=='#' then '#' else  '.' ) (zip gridRow maskRow)

applyGridMask :: [String] -> [String] -> [String]
applyGridMask grid mask = 
  map (\(gridRow,maskRow) -> (applyRowMask gridRow maskRow) ) (zip grid mask)

getRect :: [String] -> String -> [String]
getRect grid size = do
  let [xcord,ycord] = parseCmd size
  let emptyRow = take 50 $ repeat '.'
  let row = (take xcord $ repeat '#')  ++ (take (50-xcord) $ repeat '.')
  let mask = take ycord ( repeat row ) ++ (take (6-ycord) $ repeat emptyRow)
  applyGridMask grid mask

rotate :: [String] -> String -> [String]
rotate grid cmd = do
  let xcord = 1
  let working = transpose grid
  transpose $ map (\(idx, y) -> 
                      if idx==xcord then (fix y xcord) else y 
                  ) (zip [0..] $ transpose grid) 
  where fix row offset = take (length row) ((take offset $ repeat '.') ++ row)
