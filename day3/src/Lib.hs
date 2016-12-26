module Lib
    ( someFunc
      ,triStream
      ,tryTriangle
      ,triangleTest
    ) where
import Debug.Trace
import Data.Fixed
import Data.List.Split

someFunc :: IO ()
someFunc = do
  contents <- readFile "input.txt"
  let contentLines = lines contents
  let results = map (\line -> 
        case readNumbers line of
          [a, b, c]  -> tryTriangle a b c
          _ -> error "Expected 3 numbers"
        ) contentLines
  let answer = length $ filter (==True) results
  putStrLn ("answer = " ++ show answer)

  
  let (collection1, collection2, collection3) 
            = triStream $ map readNumbers contentLines

  let answer2 = triangleTest collection1 + 
                triangleTest collection2 + 
                triangleTest collection3
  putStrLn ("answer2 = " ++ show answer2)
  -- tried 3572


triangleTest :: [Int] -> Int
triangleTest collection = do
  let results = map ( \col -> tryTriangle (col!!0) (col!!1) (col!!2) )
                    (chunksOf 3 collection)
  length $ filter (==True) results

triStream :: [[a]] -> ([a],[a],[a])
triStream = foldr (\y (a, b, c) -> 
    ((head y):a, (head $tail y):b, (head $ tail $ tail y):c )) ([],[],[])

  
  
-- 213  627  601      

readNumbers :: String -> [Int]
readNumbers = map read . words

tryTriangle :: Int -> Int -> Int -> Bool
tryTriangle firstSide secondSide thirdSide = do
  let sides = [firstSide, secondSide, thirdSide]
  let longside = maximum sides
  let otherTwo = filter (\l -> l /= longside) sides
  length otherTwo < 2  || longside - ( sum otherTwo) < 0

hundredsPlace :: Int -> Int 
hundredsPlace val = ( val `div` 100 ) `mod` 10


tryTriangle2 :: Int -> Int -> Int -> Bool
tryTriangle2 firstSide secondSide thirdSide = do
 (hundredsPlace firstSide == hundredsPlace secondSide) 
   && hundredsPlace secondSide == hundredsPlace thirdSide
