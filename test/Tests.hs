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
    describe "Empty List" $ do
      it "should return a System" $ do
        Lib.parser [] `shouldBe` Lib.System {Lib.systemComponents = []}
    describe "Simple System" $ do
      it "should return a System" $
        do
          Lib.parser $ Lib.lexicalAnalysis "AwesomeReact expects an uptime of 99.9% and depends on StableBackend and ReliableAuthentication."
          `shouldBe` Lib.System
            { Lib.systemComponents =
                [ Lib.Component
                    { Lib.name = "AwesomeReact",
                      Lib.uptimeExpectation = 99.9,
                      Lib.dependencies =
                        [ "StableBackend",
                          "ReliableAuthentication"
                        ]
                    }
                ]
            }

  describe "Semantic Analysis" $ do
    describe "Empty System" $ do
      it "should return True" $ do
        Lib.semanticAnalysis Lib.System {Lib.systemComponents = []} `shouldBe` True
    describe "Invalid Simple System" $ do
      it "should return False if mentions non-existent dependencies" $ do
        Lib.semanticAnalysis
          ( Lib.System
              { Lib.systemComponents =
                  [ Lib.Component
                      { Lib.name = "AwesomeReact",
                        Lib.uptimeExpectation = 99.9,
                        Lib.dependencies =
                          [ "StableBackend",
                            "ReliableAuthentication"
                          ]
                      }
                  ]
              }
          )
          `shouldBe` False

    describe "Valid Simple System" $ do
      it "should return True if all dependencies exist" $ do
        Lib.semanticAnalysis
          ( Lib.System
              { Lib.systemComponents =
                  [ Lib.Component
                      { Lib.name = "AwesomeReact",
                        Lib.uptimeExpectation = 99.9,
                        Lib.dependencies =
                          [ "StableBackend",
                            "ReliableAuthentication"
                          ]
                      },
                    Lib.Component
                      { Lib.name = "StableBackend",
                        Lib.uptimeExpectation = 99.95,
                        Lib.dependencies = []
                      },
                    Lib.Component
                      { Lib.name = "ReliableAuthentication",
                        Lib.uptimeExpectation = 99.99,
                        Lib.dependencies = []
                      }
                  ]
              }
          )
          `shouldBe` True
