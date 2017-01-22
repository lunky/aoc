module Day8Spec where

import Test.Hspec
import Test.QuickCheck
import Day8Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Day8Lib " $ do 
    it "should call a Day8Lib function" $ do
      working `shouldBe` "working"

  describe "getRect" $ do --
    it "should return the all zero list passed in" $ do
      let input = take 6 $ repeat (take 50 $ repeat '.')
      let cmd = "0x0"
      let output = take 6 $ repeat (take 50 $ repeat '.')
      getRect input cmd `shouldBe` output

    it "should return the same list passed in" $ do
      let input = (replicate 47 '.' ++ "###") : (take 4 $ repeat (take 50 $ repeat '.')) 
      let cmd = "0x0"
      let output = (replicate 47 '.' ++ "###") : (take 4 $ repeat (take 50 $ repeat '.')) 
      getRect input cmd `shouldBe` output

    it "should return a list of 6 lists of 50 elements with 2x3 rectangle requested " $ do
      let input = [replicate 50 '.'| x <- [1..6]]
      let cmd = "3x2"
      let output = [  "###" ++ replicate 47 '.', 
                      "###" ++ replicate 47 '.', 
                      replicate 50 '.', 
                      replicate 50 '.',
                      replicate 50 '.',
                      replicate 50 '.' ]
      getRect input cmd `shouldBe` output

    it "should return the same list passed in with a 1x5" $ do
      let input = (replicate 47 '.' ++ "###") : (take 4 $ repeat (take 50 $ repeat '.')) 
      let cmd = "0x0"
      let output = (replicate 47 '.' ++ "###") : (take 4 $ repeat (take 50 $ repeat '.')) 
      getRect input cmd `shouldBe` output

  describe "rotate" $ do --
    it "should take te 3x2 rect and rotating column x=1 by 1" $ do
      let input = [ "###...............................................",
                    "###...............................................",
                    "..................................................",
                    "..................................................",
                    ".................................................."
                  ]
      let cmd = "rotate column x=1 by 1"
      let output = ["#.#...............................................",
                    "###...............................................",
                    ".#................................................",
                    "..................................................",
                    ".................................................."
                  ]
      rotate input cmd `shouldBe` output

  describe "diapatchCommand" $ do --
    it "should call getRect <grid> 3x2" $ do 
      let input = [replicate 50 '.'| x <- [1..6]]
      let output = [  "###" ++ replicate 47 '.', 
                      "###" ++ replicate 47 '.', 
                      replicate 50 '.', 
                      replicate 50 '.',
                      replicate 50 '.',
                      replicate 50 '.' ]
      let cmd = "rect 3x2"
      dispatchCommand cmd input `shouldBe` output

    it "should call getRect <grid> 2x3" $ do 
      let input = [replicate 50 '.'| x <- [1..6]]
      let output = [  "##" ++ replicate 48 '.', 
                      "##" ++ replicate 48 '.', 
                      "##" ++ replicate 48 '.', 
                      replicate 50 '.', 
                      replicate 50 '.', 
                      replicate 50 '.' ]
      let cmd = "rect 2x3"
      dispatchCommand cmd input `shouldBe` output
      
    it "should take te 3x2 rect and rotating column x=1 by 1" $ do
      let input = [ "###....",
                    "###....",
                    "......."
                  ]
      let cmd = "rotate column x=1 by 1"
      let output = ["#.#....",
                    "###....",
                    ".#....."
                  ]
      dispatchCommand cmd input `shouldBe` output

    it "should take te 3x2 rect and rotating column x=1 by 3" $ do
      let input = [ "###....",
                    "###....",
                    ".......",
                    ".......",
                    "......."
                  ]
      let cmd = "rotate column x=1 by 3"
      let output = ["#.#....",
                    "#.#....",
                    ".......",
                    ".#.....",
                    ".#....."
                  ]
      dispatchCommand cmd input `shouldBe` output

    it "should take te 3x2 rect and rotating row y=2 by 3" $ do
      let input = [ "###....",
                    "###....",
                    "......."
                  ]
      let cmd = "rotate row y=1 by 3"
      let output = ["###....",
                    "...###.",
                    "......."
                  ]
      dispatchCommand cmd input `shouldBe` output

  describe "Day8 part a run" $ do --
    it "should accept multiple commands" $ do
      let emptyGrid = [replicate 7 '.'| x <- [1..3]]
      contents <- readFile "day8input.txt"
      let contents = [  "rect 3x2", 
                        "rotate column x=1 by 1", 
                        "rotate row y=0 by 4",
                        "rotate column x=1 by 1" ] 
--      let input = lines contents
      let input = contents
      let output = foldl (\y grid -> dispatchCommand grid y ) emptyGrid input
      output `shouldBe` [ ".#..#.#",
                          "#.#....",
                          ".#....."
                        ]
    it "should read the input and produce a count of #'s" $ do
      let emptyGrid = [replicate 50 '.'| x <- [1..6]]
      contents <- readFile "day8input.txt"
      let input = lines contents
      let output = foldl (\y grid -> dispatchCommand grid y ) emptyGrid input
      (length $ filter (=='#') $ concat output) `shouldBe` 128

  describe "Day8 part a run" $ do --
    it "should read the input and produce an output message 5x6" $ do
      let emptyGrid = [replicate 50 '.'| x <- [1..6]]
      contents <- readFile "day8input.txt"
      let input = lines contents
      let output = foldl (\y grid -> dispatchCommand grid y ) emptyGrid input
      output `shouldBe` 
            ["####..##...##..###...##..###..#..#.#...#.##...##..",
             "#....#..#.#..#.#..#.#..#.#..#.#..#.#...##..#.#..#.",
             "###..#..#.#..#.#..#.#....#..#.####..#.#.#..#.#..#.",
             "#....#..#.####.###..#.##.###..#..#...#..####.#..#.",
             "#....#..#.#..#.#.#..#..#.#....#..#...#..#..#.#..#.",
             "####..##..#..#.#..#..###.#....#..#...#..#..#..##.."]


