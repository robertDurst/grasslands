module Main (main) where

import Lib qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Lexical Analysis" $ do
    it "should return a list of tokens" $ do
      Lib.lexicalAnalysis "" `shouldBe` []

  describe "Parser" $ do
    it "should return an AST" $ do
      Lib.parser [] `shouldBe` ""

  describe "Semantic Analysis" $ do
    it "should return an AST" $ do
      Lib.semanticAnalysis "" `shouldBe` ""

  describe "IR Generation" $ do
    it "should return an IR" $ do
      Lib.irGeneration "" `shouldBe` ""

  describe "Optimization" $ do
    it "should return an IR" $ do
      Lib.optimization "" `shouldBe` ""

  describe "Code Generation" $ do
    it "should return a String" $ do
      Lib.codeGeneration "" `shouldBe` ""

  describe "Linking and Assembly" $ do
    it "should return a String" $ do
      Lib.linkingAndAssembly "" `shouldBe` ""
