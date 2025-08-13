module Main
  ( lexicalAnalysis,
    parser,
    semanticAnalysis,
    main,
    makeComponent,
    Token (..),
    Component (..),
    System (..),
  )
where

import Lexer (Token (..), lexicalAnalysis)
import Parser (Component (..), System (..), makeComponent, parser)
import SemanticAnalyzer (semanticAnalysis)

main :: IO ()
main = do
  putStrLn "Welcome to the Grasslands Compiler!"