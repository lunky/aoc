import Test.Hspec
import Test.QuickCheck
import Lib

main :: IO ()
main = hspec $ do
  describe "Lib" $ do 
    describe "Test harness" $ do 
        it "demonstrates that tests are working" $ do
          1 `shouldBe` 1
    describe "tryTriangle" $ do 
      it "should return False if not valid triangle" $ do
        tryTriangle 5 10 25 `shouldBe` False
        tryTriangle 5 19 25 `shouldBe` False
        tryTriangle 5 5 10 `shouldBe` False

      it "should return true if valid triangle" $ do
        tryTriangle 8 8 5  `shouldBe` True
        tryTriangle 5 21 25 `shouldBe` True
        tryTriangle 5 5 2 `shouldBe` True
        tryTriangle 3 3 3 `shouldBe` True
  
    describe "tristream" $ do
      it "should return three vertical collections" $ do
        let input = [[1,2,3],[1,2,3],[1,2,3],[1,2,3]]
        let output = ([1,1,1,1],[2,2,2,2],[3,3,3,3])
        triStream input `shouldBe` output

