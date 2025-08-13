module Lib
  ( lexicalAnalysis,
    parser,
    semanticAnalysis,
    irGeneration,
    optimization,
    codeGeneration,
    linkingAndAssembly,
    Token (..),
    AST,
    IR,
  )
where

import Lexer (Token (..), lexicalAnalysis)

type AST = String

type IR = String

-- Step 2: Parsing
parser :: [Token] -> AST
parser _ = ""

-- Step 3: Semantic Analysis and Type Checking
semanticAnalysis :: AST -> AST
semanticAnalysis _ = ""

-- Step 4: IR Generation
irGeneration :: AST -> IR
irGeneration _ = ""

-- Step 5: Optimization
optimization :: IR -> IR
optimization _ = ""

-- Step 6: Code Generation
codeGeneration :: IR -> String
codeGeneration _ = ""

-- Step 7: Linking and Assembly
linkingAndAssembly :: String -> String
linkingAndAssembly _ = ""
