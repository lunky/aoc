{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
import Test.Hspec
import Test.QuickCheck
import Day5Lib
import Day6Lib
import qualified Data.List

main :: IO ()
main = hspec $ do
  describe "Day5Lib" $ do 
    describe "Test harness" $ do 
        it "demonstrates that tests are working" $ do
          1 `shouldBe` 1

    describe "findFiveZeroHash" $ do 
      it "should find a hash with 5 leading zeros" $ do
        let input = "abc"
--        let output = [3231929, 5017308, 5278568]
--        findFiveZeroHash input 3 `shouldBe` output
--        let output = [3231929]
--        findFiveZeroHash input 1 `shouldBe` output
        pendingWith "takes too long to run, uncomment if neccesary"

    describe "getSixthDigit" $ do 
      it "should find the 6th character in a hash from the key" $ do
        let input = "abc3231929"
        let output = '1'
        getSixthDigit input `shouldBe` output

    describe "runs all 8 and gets the password" $ do 
      it "should run give the answer" $ do
        let input = "cxdnnyjw"
        let output = "f77a0e6e"
        --let col = findFiveZeroHash input 8
        --map (\y -> getSixthDigit (input ++ show y)) col `shouldBe` output
        pendingWith "takes too long to run, uncomment if neccesary"

    describe "getSixthAndSeventhDigit" $ do 
      it "should find the 6th and 7th character in a hash from the key" $ do
        let input = "abc3231929"
        let output = ('1', '5')
        getSixthAndSeventhDigit input `shouldBe` output


    describe "runs all 8 and gets the password to part b" $ do 
      it "should run give the answer to part b" $ do
        let input = "cxdnnyjw"
        let output = "999828ec"
--        day5Decrypt input `shouldBe` output
        pendingWith "takes too long to run, uncomment if neccesary"

      it "should run give the unsorted answer to part b" $ do
        let input = "abc"
        let output = "05ace8e3"
--        day5Decrypt input `shouldBe` output
        pendingWith "takes too long to run, uncomment if neccesary"

