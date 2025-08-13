module Tests (main) where

import Control.Exception (evaluate)
import Main qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Lexical Analysis" $ do
    describe "Empty String" $ do
      it "should return an empty list for an empty string" $ do
        Main.lexicalAnalysis "" `shouldBe` []
    describe "Words" $ do
      it "should tokenize words" $ do
        Main.lexicalAnalysis "hello world" `shouldBe` [Main.WordToken "hello", Main.WordToken "world"]
    describe "Periods" $ do
      it "should tokenize period" $ do
        Main.lexicalAnalysis "." `shouldBe` [Main.PeriodToken]
    describe "Percentages" $ do
      it "should throw error for empty percentages" $ do
        evaluate (Main.lexicalAnalysis "%") `shouldThrow` anyErrorCall
      it "should tokenize int percentage" $ do
        Main.lexicalAnalysis "1%" `shouldBe` [Main.PercentageToken 1]
      it "should tokenize float percentage" $ do
        Main.lexicalAnalysis "1.1%" `shouldBe` [Main.PercentageToken 1.1]

  describe "Parser" $ do
    describe "Empty List" $ do
      it "should return a System" $ do
        Main.parser [] `shouldBe` Main.System {Main.systemComponents = []}
    describe "Simple System" $ do
      it "should return a System" $
        do
          Main.parser $ Main.lexicalAnalysis "AwesomeReact expects an uptime of 99.9% and depends on StableBackend and ReliableAuthentication."
          `shouldBe` Main.System
            { Main.systemComponents =
                [ Main.makeComponent "AwesomeReact" 99.9 ["StableBackend", "ReliableAuthentication"]
                ]
            }

  describe "Semantic Analysis" $ do
    describe "Empty System" $ do
      it "should return True" $ do
        Main.semanticAnalysis Main.System {Main.systemComponents = []} `shouldBe` True
    describe "Invalid Simple System" $ do
      it "should return False if mentions non-existent dependencies" $ do
        Main.semanticAnalysis
          ( Main.System
              { Main.systemComponents =
                  [ Main.makeComponent "AwesomeReact" 99.9 ["StableBackend", "ReliableAuthentication"],
                    Main.makeComponent "StableBackend" 99.95 []
                  ]
              }
          )
          `shouldBe` False

    describe "Valid Simple System" $ do
      it "should return True if all dependencies exist" $ do
        Main.semanticAnalysis
          ( Main.System
              { Main.systemComponents =
                  [ Main.makeComponent "AwesomeReact" 99.9 ["StableBackend", "ReliableAuthentication"],
                    Main.makeComponent "StableBackend" 99.95 [],
                    Main.makeComponent "ReliableAuthentication" 99.99 []
                  ]
              }
          )
          `shouldBe` True
