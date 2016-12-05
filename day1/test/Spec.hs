import Test.Hspec
import Test.QuickCheck
import Lib

main :: IO ()
main = hspec $ do

  describe "Lib" $ do 
    describe "parseSteps fn" $ do
      it "should parse steps string" $ do
        let input = "R2, L3"
        let expected = [('R',2), ('L', 3)]
        parseSteps input `shouldBe` expected

    describe "move fn" $ do
      it "should calculate (3,0)" $ do
        let inputState = ('U', (0,0))
        let input = ('R', 3)
        let expected = ('R', (3,0))
        move input inputState `shouldBe` expected
      it "should calculate (3,-3)" $ do
        let inputState = ('R', (3,0))
        let input = ('R', 3)
        let expected = ('D', (3,-3))
        move input inputState `shouldBe` expected
      it "should calculate (0,-3)" $ do
        let inputState = ('D', (3,-3))
        let input = ('R', 3)
        let expected = ('L', (0,-3))
        move input inputState `shouldBe` expected

      it "should calculate (-3,-3)" $ do
        let inputState = ('L', (-3, 0))
        let input = ('L', 3)
        let expected = ('D', (-3,-3))
        move input inputState `shouldBe` expected

      it "should calculate R2 => (2,0)" $ do
        move ('R', 2) ('U', (0,0)) `shouldBe` ('R', (2,0))
      it "should calculate L3 => (2,3)" $ do
        move ('L', 3) ('R', (2,0)) `shouldBe` ('U', (2,3))

    describe "followInstructions fn" $ do

      it "should calculate ('R', (2,0))" $ do
        let input = "R2"
        let expected = ('R', (2,0))
        followInstructions input `shouldBe` expected

      it "should calculate ('L', (-2,0))" $ do
        let input = "L2"
        let expected = ('L', (-2,0))
        followInstructions input `shouldBe` expected

      it "should calculate ('L', (-2,-2))" $ do
        let input = "L2, L2"
        let expected = ('D', (-2,-2))
        followInstructions input `shouldBe` expected

      it "should calculate ('R', (0,-2))" $ do
        let input = "L2, L2, L2"
        let expected = ('R', (0,-2))
        followInstructions input `shouldBe` expected

      it "should calculate (2,3)" $ do
        let input = "R2, L3"
        let expected = ('U', (2,3))
        followInstructions input `shouldBe` expected

      it "should calculate (4,3)" $ do
        let input = "R4, L3"
        let expected = ('U', (4,3))
        followInstructions input `shouldBe` expected
