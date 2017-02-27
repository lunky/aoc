module Day10Spec where

import Test.Hspec
import Test.QuickCheck
import Day10Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Day10Lib " $ do 
    it "should call a Day10Lib function" $ do
      working `shouldBe` "working"

  let rules = ["value 5 goes to bot 2",
                "bot 2 gives low to bot 1 and high to bot 0",
                "value 3 goes to bot 1",
                "bot 1 gives low to output 1 and high to bot 0",
                "bot 0 gives low to output 2 and high to output 0",
                "value 2 goes to bot 2"]

  describe "Rules engine" $ do
    it "should parse some instructions 1" $ do 
      let input = "bot 2 gives low to bot 1 and high to bot 0"
      let output = Inst 2 Bot 1 Bot 0
      parseInstruction input `shouldBe` output

    it "should parse some instructions 2" $ do 
      let input = "bot 1 gives low to output 1 and high to bot 0"
      let output = Inst 1 Output 1 Bot 0
      parseInstruction input `shouldBe` output

