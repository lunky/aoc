module Day7Lib
    ( someFunc
      ,working
      ,isTLS
      ,isSSL
      ,parseIpAddress 
    ) where

import Text.Regex.PCRE
import Data.List
import Data.List.Split
import AocCommon

working = "working"

someFunc :: IO ()
someFunc = do
        let answer2 = "no answer"
        putStrLn $ "day7 part b: " ++ answer2

hasABBA :: String -> Bool
hasABBA str = do
  let abba = "(.)(?!\\1)(.)\\2\\1"
  str =~ abba

isTLS :: String -> Bool
isTLS str = do
  let aBBA = "(.)(?!\\1)(.)\\2\\1"
  let first = str =~ aBBA :: Bool 
  let hypernets = getAllTextMatches $ str =~ "\\[.*?\\]" :: [String]
  let answers = map (\y -> y =~ aBBA :: Bool ) hypernets
  first && (and $ map (==False) answers)


parseIpAddress ipAddress = do
  let [hypernets,supernets] =  splitOddsAndEvens $ splitOneOf "[]" ipAddress
  (hypernets,supernets) 

isSSL :: String -> Bool
isSSL str = do
  let (hypernets,supernets) =  parseIpAddress str
  let toFindBABs = foldl (\x y -> (getABAs y)++x  ) [] hypernets
  any (\x -> any (\y -> isInfixOf y x ) toFindBABs ) supernets

getABAs :: String -> [String]
getABAs str =  do
  if length str < 3 then [] else
    filter (/="") $ map (\[a,b,c] -> if a==c && a/=b then [b,a,b] else []  ) $ windowed 3 str
  
