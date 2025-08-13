module Lib
  ( lexicalAnalysis,
    parser,
    semanticAnalysis,
    Token (..),
    Component (..),
    System (..),
  )
where

import Lexer (Token (..), lexicalAnalysis)
import Parser (Component (..), System (..), parser)
import SemanticAnalyzer (semanticAnalysis)
