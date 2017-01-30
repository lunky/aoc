module Day9Spec where

import Test.Hspec
import Test.QuickCheck
import Day9Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Day9Lib " $ do 
    it "should call a Day9Lib function" $ do
      working `shouldBe` "working"

  describe "decompress" $ do 
    it "should decompress a string w/o compression" $ do 
      let input = "ADVENT"
      let output = "ADVENT"
      decompress input `shouldBe` output

    it "should decompress to a length of 6" $ do 
      let input = "ADVENT"
      let output = 6
      (length $ decompress input) `shouldBe` output

    it "should decompress a 1x5 pattern" $ do 
      let input = "(1x5)B"
      let output = "BBBBB"
      decompress input `shouldBe` output

    it "should decompress a 2x5 pattern" $ do 
      let input = "A(2x5)BAZ"
      let output = "ABABABABABAZ"
      decompress input `shouldBe` output

    it "should decompress a 1x5 pattern with prefix" $ do 
      let input = "AA(1x5)B"
      let output = "AABBBBB"
      decompress input `shouldBe` output

    it "should decompress a 1x5 pattern with a suffix and prefix" $ do 
      let input = "AA(1x5)BC"
      let output = "AABBBBBC"
      decompress input `shouldBe` output

    it "should decompress a 1x5 pattern with an overlap with another (1x5) " $ do 
      let input = "AA(1x5)(1x5)C"
      let output = "AA(((((1x5)C"
      decompress input `shouldBe` output

    it "should decompress pattern 2" $ do 
      let input = "A(1x5)BC"
      let output = "ABBBBBC" 
      decompress input `shouldBe` output

    it "should decompress pattern 2 with length" $ do 
      let input = "A(1x5)BC"
      let output = 7 
      (length $ decompress input) `shouldBe` output

    it "should decompress pattern 3" $ do 
      let input = "(3x3)XYZ"
      let output = "XYZXYZXYZ"
      decompress input `shouldBe` output

    it "should decompress pattern 3 with length" $ do 
      let input = "(3x3)XYZ"
      let output = 9
      (length $ decompress input) `shouldBe` output

    it "should decompress pattern 4" $ do 
      let input = "A(2x2)BCD(2x2)EFG"
      let output = "ABCBCDEFEFG"
      decompress input `shouldBe` output

    it "should decompress pattern 4 with length" $ do 
      let input = "A(2x2)BCD(2x2)EFG"
      let output = 11
      (length $ decompress input) `shouldBe` output

    it "should decompress pattern 5" $ do 
      let input = "(6x1)(1x3)A" 
      let output = "(1x3)A"
      decompress input `shouldBe` output

    it "should decompress pattern 5" $ do 
      let input = "X(8x2)(3x3)ABCY"
      let output = "X(3x3)ABC(3x3)ABCY"
      decompress input `shouldBe` output

    it "should decompress two patterns" $ do 
      let input = "X(1x2)**(3x3)ABCY"
      let output = "X***ABCABCABCY"
      decompress input `shouldBe` output

  describe "day9 a answer" $ do
    it "should produce the correct answer" $ do
      contents <- readFile "day9input.txt"
      let input = head $ lines contents
      let output = 152851
      ( length $ decompress $ input ) `shouldBe` output

  describe "decompressB" $ do 
    it "should decompress using 2nd format" $ do
      let input="X(8x2)(3x3)ABCY"
      let output="XABCABCABCABCABCABCY"
      (decompress2 input) `shouldBe` output

    it "should decompress using 2nd format" $ do
      let input="(27x12)(20x12)(13x14)(7x10)(1x12)A"
      let output=241920
      length (decompress2 input) `shouldBe` output
      
    it "should decompress using 2nd format" $ do
      let input="(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"
      let output=445
      length ( decompress2 input) `shouldBe` output

  describe "day9 b answer" $ do
    it "should produce the correct answer" $ do
      contents <- readFile "day9input.txt"
      let input = head $ lines contents
      let output = 152851
      ( length $ decompress2 $ input ) `shouldBe` output
