import Test.Hspec
import Test.QuickCheck
import Lib

main :: IO ()
main = hspec $ do
  describe "Lib" $ do 
    describe "Test harness" $ do 
        it "demonstrates that tests are working" $ do
          1 `shouldBe` 1
    describe "followInstructions" $ do 
      it "follows ULL to get 1" $ do 
        followInstructions 5 "ULL" `shouldBe` 1
      it "follows 1 RRDDD to get 9" $ do 
        followInstructions 1 "RRDDD" `shouldBe` 9
      it "follows 9 LURDL to get 8" $ do 
        followInstructions 9 "LURDL" `shouldBe` 8
      it "follows 8 UUUUD to get 5" $ do 
        followInstructions 8 "UUUUD" `shouldBe` 5

    describe "followInstructionsStar" $ do 
      it "follows ULL to get 1" $ do 
        followInstructionsStar '7' "ULL" `shouldBe` '2'


    describe "processLines" $ do 
      it "instructions with \\n in them should produce new int" $ do 
        processLines "ULL\nRRDDD" `shouldBe` "19"

      it "instructions with \\n in them should produce new int 2" $ do 
        processLines "ULL\nRRDDD\nLURDL" `shouldBe` "198"

      it "multiline instructions produce new code" $ do 
        processLines "ULL\nRRDDD\nLURDL\nUUUUD" `shouldBe` "1985"
    describe "moveStar" $ do
      it "will return new elem" $ do
        moveStar 'U' '1' `shouldBe` '1'
        moveStar 'U' '7' `shouldBe` '3'
        moveStar 'L' '3' `shouldBe` '2'

    describe "processLines star" $ do 
      it "instructions with \\n in them should produce new int" $ do 
        processLinesStar "ULL" `shouldBe` "2"
        processLinesStar "ULL\nRRDDD" `shouldBe` "2C"
