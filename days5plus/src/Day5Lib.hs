module Day5Lib
    ( someFunc
      ,generatePassword
      ,findFiveZeroHash
      ,getSixthDigit
    ) where

import qualified Data.ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Char
import qualified Data.ByteString.Base16 as Base16
import qualified Crypto.Hash.MD5 as MD5

someFunc :: IO ()
someFunc = do
        let input = "cxdnnyjw"
        let col = findFiveZeroHash input 8
        let answer = map (\y -> getSixthDigit (input ++ show y)) col 
        putStrLn $ "day5 part a: " ++ answer

getHash :: [Char] -> [Char]
getHash str = do 
  Char8.unpack . Base16.encode . MD5.hash $ Char8.pack str


startsWithFiveZeros :: String -> Bool
startsWithFiveZeros = startsWith' "00000" 
  where
    startsWith' [] _ = True
    startsWith' _ [] = False
    startsWith' (hs:ts) (hl:tl) 
      | hs == hl = startsWith' ts tl
      | otherwise = False

hashStartsWithFiveZeros :: String -> Int -> Bool
hashStartsWithFiveZeros pass num = 
  startsWithFiveZeros $ getHash  $ pass ++ (show num)

findFiveZeroHash :: String -> Int -> [Int]
findFiveZeroHash pass howMany = 
  take howMany $ generatePassword pass

generatePassword :: [Char] -> [Int] 
generatePassword pass = do 
  [x | x <- [0..] , (hashStartsWithFiveZeros pass x) ]

getSixthDigit :: String -> Char
getSixthDigit pass = (getHash pass)!!5
   
