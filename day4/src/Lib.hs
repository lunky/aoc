module Lib
    ( someFunc
      ,sumSectorIds
      ,testRealRoom
      ,parseName
      ,frequency
      ,getChecksum
      ,getChecksum'
      ,decryptName
    ) where

import Data.List
import Data.List.Split

sumSectorIds contents = do
  let contentLines = lines contents
  foldr (\y acc -> acc + (testRealRoom y)) 0 contentLines


someFunc :: IO ()
someFunc = do
  contents <- readFile "input.txt"
  let answer = sumSectorIds contents
  putStrLn $ "day4 part a: " ++ show answer
  let answer2 = map (\line -> decryptName line ) (lines contents)
  mapM_ print answer2
  putStrLn  "day4 part a: " 

testRealRoom :: [Char] -> Int
testRealRoom str = do
  let (name, sector, checksum) = parseName str
  let answer = take 5 $ getChecksum name
  if answer==checksum
  then read sector
  else 0

parseName str = do
  let [longname, checkSum] = splitOn "[" str
  let name = tail $ dropWhile (/='-') $ reverse longname
  let sector = reverse $ takeWhile (/='-') $ reverse longname
  (reverse $ name, sector, init checkSum)

frequency str = map (\x -> (head x, length x)) $ group $ sort str

sortGT (a1, b1) (a2, b2)  
  | b1 > b2 = LT 
  | b1 < b2 = GT 
  | b1 == b2 = compare a1 a2 

getChecksum str = map (\(y,_) -> y) $ getChecksum' str

getChecksum' str = Data.List.sortBy sortGT $ frequency $ filter (/='-') str


shiftCipher c  places = do
    let key = ['a'..'z']
    let idx = elemIndex c key
    if c=='-' then ' ' else do
      let el = case idx of
                Just value ->  key!!((value+places) `mod` 26)
                Nothing -> error "invalid char"
      el

decryptName str = do
  let (name, sector, checksum) = parseName str
  map (\y -> shiftCipher y (read sector) ) name 
    

