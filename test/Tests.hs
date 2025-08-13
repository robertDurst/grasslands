module Tests (main) where

import Control.Exception (evaluate)
import Main (Component (..), System (..), Token (..), lexicalAnalysis, makeComponent, parser, semanticAnalysis, verifySystem)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Lexical Analysis" $ do
    describe "Empty String" $ do
      it "should return an empty list for an empty string" $ do
        lexicalAnalysis "" `shouldBe` []
    describe "Words" $ do
      it "should tokenize words" $ do
        lexicalAnalysis "hello world" `shouldBe` [WordToken "hello", WordToken "world"]
    describe "Periods" $ do
      it "should tokenize period" $ do
        lexicalAnalysis "." `shouldBe` [PeriodToken]
    describe "Percentages" $ do
      it "should throw error for empty percentages" $ do
        evaluate (lexicalAnalysis "%") `shouldThrow` anyErrorCall
      it "should tokenize int percentage" $ do
        lexicalAnalysis "1%" `shouldBe` [PercentageToken 1]
      it "should tokenize float percentage" $ do
        lexicalAnalysis "1.1%" `shouldBe` [PercentageToken 1.1]

  describe "Parser" $ do
    describe "Empty List" $ do
      it "should return a System" $ do
        parser [] `shouldBe` System {systemComponents = []}
    describe "Simple System" $ do
      it "should return a System" $
        do
          parser $ lexicalAnalysis "AwesomeReact expects an uptime of 99.9% and depends on StableBackend and ReliableAuthentication."
          `shouldBe` System
            { systemComponents =
                [ makeComponent "AwesomeReact" 99.9 ["StableBackend", "ReliableAuthentication"]
                ]
            }
    describe "Component with no dependencies" $ do
      it "should parse 'depends on nothing' as empty dependency list" $
        do
          parser $ lexicalAnalysis "Database expects an uptime of 99.99% and depends on nothing."
          `shouldBe` System
            { systemComponents =
                [ makeComponent "Database" 99.99 []
                ]
            }

  describe "Semantic Analysis" $ do
    describe "Empty System" $ do
      it "should return True" $ do
        semanticAnalysis System {systemComponents = []} `shouldBe` True
    describe "Invalid Simple System" $ do
      it "should return False if mentions non-existent dependencies" $ do
        semanticAnalysis
          ( System
              { systemComponents =
                  [ makeComponent "AwesomeReact" 99.9 ["StableBackend", "ReliableAuthentication"],
                    makeComponent "StableBackend" 99.95 []
                  ]
              }
          )
          `shouldBe` False

    describe "Valid Simple System" $ do
      it "should return True if all dependencies exist" $ do
        semanticAnalysis
          ( System
              { systemComponents =
                  [ makeComponent "AwesomeReact" 99.9 ["StableBackend", "ReliableAuthentication"],
                    makeComponent "StableBackend" 99.95 [],
                    makeComponent "ReliableAuthentication" 99.99 []
                  ]
              }
          )
          `shouldBe` True

  describe "Verify System" $ do
    describe "Valid System" $ do
      it "should return True if all dependencies exist" $ do
        input <- readFile "examples/simple_system.grass"
        verifySystem input `shouldBe` True

    describe "Invalid System" $ do
      it "should return False if mentions non-existent dependencies" $ do
        input <- readFile "examples/invalid_simple_system.grass"
        verifySystem input `shouldBe` False
