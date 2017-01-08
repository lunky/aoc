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

    it "should take te 3x2 rect and rotating column x=1 by 1" $ do
      let input = [ "###...............................................",
                    "###...............................................",
                    "..................................................",
                    "..................................................",
                    ".................................................."
                  ]
      let cmd = "column x=1 by 1"
      let output = ["#.#...............................................",
                    "###...............................................",
                    ".#................................................",
                    "..................................................",
                    ".................................................."
                  ]
      rotate input cmd `shouldBe` output

