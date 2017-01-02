module Day6Lib
    ( someFunc
      ,day6a
      ,day6b
      ,working
    ) where

import Data.List

someFunc :: IO ()
someFunc = do
        print "not yet"

working = "working"

mostFrequent :: [Char] ->  Char
mostFrequent lst = fst $ 
            head $ sortBy (\a b -> compare (snd b) (snd a)) 
            $ frequency lst

leastFrequent :: [Char] ->  Char
leastFrequent lst = fst $ 
            head $ sortBy (\a b -> compare (snd a) (snd b)) 
            $ frequency lst

day6a content = do 
  let rows = transpose $ lines content
  map mostFrequent rows

day6b content = do 
  let rows = transpose $ lines content
  map leastFrequent rows

frequency str = map (\x -> (head x, length x)) $ group $ sort str
