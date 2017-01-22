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



applyGridMask :: [String] -> [String] -> [String]
applyGridMask grid mask = 
  map (\(gridRow,maskRow) -> (applyRowMask gridRow maskRow) ) (zip grid mask)
  where applyRowMask :: String -> String -> String
        applyRowMask gridRow maskRow = 
          map (\(gridChar,maskChar) -> 
            if gridChar=='#' || maskChar=='#' then '#' else  '.' ) (zip gridRow maskRow)

getRect :: [String] -> String -> [String]
getRect grid size = do
  let [xcord,ycord] = parseRec size
  let emptyRow = take (length $ head grid) $ repeat '.'
  let row = (take xcord $ repeat '#')  ++ (take ((length emptyRow)-xcord) $ repeat '.')
  let mask = take ycord ( repeat row ) ++ (take ((length grid)-ycord) $ repeat emptyRow)
  applyGridMask grid mask
  where 
  parseRec :: String -> [Int]
  parseRec = map read . splitOn "x" 

rotate :: [String] -> String -> [String]
rotate grid cmd = do
  let (command, rowOrColumn, coord, howMuch) = parseCommand $ cmd
  rotate' rowOrColumn grid coord howMuch
  where
  rotate' rowOrColumn grid coord howMuch 
    | rowOrColumn == "column" =  transpose $ rotate' "row" (transpose grid) coord howMuch
    | rowOrColumn == "row" =  map (\(idx, y) ->
                                    if idx==coord then (fix y howMuch) else y 
                                  ) (zip [0..] $ grid) 
    where fix row offset = drop (length row - offset) row ++ take (length row - offset) row

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
