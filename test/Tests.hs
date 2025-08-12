module Main (main) where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "foobar" $ do
    it "should be foobar" $ do
      "foobar" `shouldBe` "foobar"
