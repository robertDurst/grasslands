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

import Lexer (Token (..), lexicalAnalysis)
import Parser (Component (..), System (..), makeComponent, parser)
import SemanticAnalyzer (semanticAnalysis)

verifySystem :: String -> Bool
verifySystem = semanticAnalysis . parser . lexicalAnalysis

main :: IO ()
main = do
  putStrLn "Welcome to the Grasslands Compiler!"