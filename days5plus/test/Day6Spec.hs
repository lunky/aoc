module Day6Spec where

import Test.Hspec
import Test.QuickCheck
import Day6Lib
import qualified Data.List

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Day6Lib " $ do 
    it "should call a Day6Lib function" $ do
      working `shouldBe` "working"

    describe "day6a" $ do
      it "should find 'easter'" $ do 
        let input = "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar"
        let output = "easter"
        day6a input `shouldBe` output

      it "should find answer get the answer to part a'" $ do 
        contents <- readFile "day6input.txt"
        let input = contents
        let output = "tzstqsua"
        day6a input `shouldBe` output

      it "should find 'advent'" $ do 
        let input = "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar"
        let output = "advent"
        day6b input `shouldBe` output

      it "should find answer get the answer to part b'" $ do 
        contents <- readFile "day6input.txt"
        let input = contents
        let output = "myregdnr"
        day6b input `shouldBe` output
