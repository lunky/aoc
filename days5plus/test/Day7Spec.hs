module Day7Spec where

import Test.Hspec
import Test.QuickCheck
import Day7Lib
import qualified Data.List

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Day7Lib " $ do 
    it "should call a Day7Lib function" $ do
      working `shouldBe` "working"

    describe "isTLS" $ do
      it "should identify a valid TLS IP address" $ do
        let input = "abba[mnop]qrst"
        let output = True
        isTLS input `shouldBe` output

      it "should identify a valid TLS IP address" $ do
        let input = "ioxxoj[asdfgh]zxcvbn"
        let output = True
        isTLS input `shouldBe` output

      it "should identify a non TLS IP address" $ do 
        let input = "abcd[bddb]xyyx"
        let output = False
        isTLS input `shouldBe` output

      it "2 should identify a non TLS IP address" $ do 
        let input = "abcd[bddb]xyyx[ssde]blah"
        let output = False
        isTLS input `shouldBe` output
        
      it "should identify a TLS IP address" $ do 
        let input = "abcd[bsdb]xyyx[ssde]blah"
        let output = True
        isTLS input `shouldBe` output

    describe "day7 a answer" $ do
      it "should produce the correct answer" $ do
        contents <- readFile "day7input.txt"
        let input = contents
        let output = 105 
        ( length $ filter isTLS $ lines input ) `shouldBe` output
        
    describe "isSSL" $ do 
      it "should identify valid SSL address" $ do
        let input = "aba[bab]xyz"
        let output = True
        isSSL input `shouldBe` output
        
      it "should identify not valid SSL address" $ do 
        let input = "xyx[xyx]xyx"
        let output = False
        isSSL input `shouldBe` output

      it "should identify not valid SSL address" $ do 
        let input = "zeebynirxqrjbdqzjav[cawghcfvfeefkmx]xqcdkvawumyayfnq[qhhwzlwjvjpvyavtm]sbnvwssglfpyacfbua[wpbknuubmsjjbekkfy]icimffaoqghdpvsbx"
        let output = False
        isSSL input `shouldBe` output

      it "should identify not valid SSL address" $ do 
        let input = "zazbz[bzb]cdb"
        let output = True 
        isSSL input `shouldBe` output

      it "should identify valid SSL address" $ do 
        let input = "zazbz[bszb]cdb[sbzbd]dl"
        let output = True 
        isSSL input `shouldBe` output

    describe "day7 b answer" $ do
      it "should produce the correct answer" $ do
        contents <- readFile "day7input.txt"
        let input = contents
        let output = 258 
        ( length $ filter isSSL $ lines input ) `shouldBe` output

      describe "parseIPAddress" $ do
        it "should split an ip address into nets" $ do
            let input = "zeebynirxqrjbdqzjav[cawghcfvfeefkmx]xqcdkvawumyayfnq[qhhwzlwjvjpvyavtm]sbnvwssglfpyacfbua[wpbknuubmsjjbekkfy]icimffaoqghdpvsbx"
            let output = (["zeebynirxqrjbdqzjav", "xqcdkvawumyayfnq", "sbnvwssglfpyacfbua", "icimffaoqghdpvsbx"],
                            ["cawghcfvfeefkmx", "qhhwzlwjvjpvyavtm", "wpbknuubmsjjbekkfy"])
            parseIpAddress input `shouldBe` output
