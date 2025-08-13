module Main
  ( lexicalAnalysis,
    parser,
    semanticAnalysis,
    main,
    makeComponent,
    verifySystem,
    Token (..),
    Component (..),
    System (..),
  )
where

import Component (Component (..), makeComponent)
import Lexer (Token (..), lexicalAnalysis)
import Parser (parser)
import SemanticAnalyzer (semanticAnalysis)
import System (System (..))

verifySystem :: String -> Bool
verifySystem = semanticAnalysis . parser . lexicalAnalysis

main :: IO ()
main = do
  putStrLn "Welcome to the Grasslands Compiler!"