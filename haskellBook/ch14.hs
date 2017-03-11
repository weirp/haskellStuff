module Ch14 where

import Test.Hspec
import WordNumber (digitToWord, digits, wordNumber)

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      print "???"
