module Main (main) where

import Control.Exception (evaluate)
import Lib qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Lexical Analysis" $ do
    describe "Empty String" $ do
      it "should return an empty list for an empty string" $ do
        Lib.lexicalAnalysis "" `shouldBe` []
    describe "Words" $ do
      it "should tokenize words" $ do
        Lib.lexicalAnalysis "hello world" `shouldBe` [Lib.WordToken "hello", Lib.WordToken "world"]
    describe "Periods" $ do
      it "should tokenize period" $ do
        Lib.lexicalAnalysis "." `shouldBe` [Lib.PeriodToken]
    describe "Percentages" $ do
      it "should throw error for empty percentages" $ do
        evaluate (Lib.lexicalAnalysis "%") `shouldThrow` anyErrorCall
      it "should tokenize int percentage" $ do
        Lib.lexicalAnalysis "1%" `shouldBe` [Lib.PercentageToken 1]
      it "should tokenize float percentage" $ do
        Lib.lexicalAnalysis "1.1%" `shouldBe` [Lib.PercentageToken 1.1]

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
