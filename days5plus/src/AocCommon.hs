module AocCommon
    (
      windowed
      ,splitOddsAndEvens
      ,readNumbers
    ) where

windowed :: Int -> [a] -> [[a]] 
windowed size ls@(x:xs) = take size ls : if size < length ls then windowed size xs else []

splitOddsAndEvens :: [a] -> [[a]]
splitOddsAndEvens = foldr (\x [ys, zs] -> [x : zs, ys]) [[], []]

readNumbers :: String -> [Int]
readNumbers = map read . words
