module Main where

import Grasslands
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Grasslands" $ do
    describe "greet" $ do
      it "greets a person by name" $ do
        greet "World" `shouldBe` "Hello, World!"

      it "greets another person" $ do
        greet "Alice" `shouldBe` "Hello, Alice!"

    describe "add" $ do
      it "adds two positive numbers" $ do
        add 2 3 `shouldBe` 5

      it "adds zero" $ do
        add 5 0 `shouldBe` 5
