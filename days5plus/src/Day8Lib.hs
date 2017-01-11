module Day8Lib
    (
     working
     ,dispatchCommand
     ,getRect
     ,rotate
    ) where
import AocCommon
import Data.List
import Data.List.Split
import Debug.Trace

working = "working"


applyRowMask :: String -> String -> String
applyRowMask gridRow maskRow = 
  map (\(gridChar,maskChar) -> 
    if gridChar=='#' || maskChar=='#' then '#' else  '.' ) (zip gridRow maskRow)

applyGridMask :: [String] -> [String] -> [String]
applyGridMask grid mask = 
  map (\(gridRow,maskRow) -> (applyRowMask gridRow maskRow) ) (zip grid mask)

getRect :: [String] -> String -> [String]
getRect grid size = do
  let [xcord,ycord] = parseRec size
  let emptyRow = take 50 $ repeat '.'
  let row = (take xcord $ repeat '#')  ++ (take (50-xcord) $ repeat '.')
  let mask = take ycord ( repeat row ) ++ (take (6-ycord) $ repeat emptyRow)
  applyGridMask grid mask
  where 
  parseRec :: String -> [Int]
  parseRec = map read . splitOn "x" 

rotate :: [String] -> String -> [String]
rotate grid cmd = do
  let (command, rowOrColumn, coord, howMuch) = parseCommand $ cmd

  transpose $ map (\(idx, y) ->
                      if idx==coord then (fix y howMuch) else y 
                  ) (zip [0..] $ transpose grid) 
  where fix row offset = take (length row) ((take offset $ repeat '.') ++ row)

parseCommand cmd = do
  --e.g. ["rotate","row","y=12230","by","2"]
  let [rawCommand, rowOrColumn, rawCord,_,rawHowMuch] = words cmd
  (rawCommand, rowOrColumn, (read $ drop 2 rawCord), read rawHowMuch)

dispatchCommand :: String -> [String] -> [String]
dispatchCommand cmd currentGrid = do
  let cmdWords = words cmd
  dispatch' cmdWords currentGrid
  where 
    dispatch' :: [String] -> [String] -> [String]
    dispatch' ["rect",size] curr = getRect curr  size
    dispatch' cmd curr = rotate curr $ unwords cmd
