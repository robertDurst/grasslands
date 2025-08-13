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

import Data.Char (isAlpha, isDigit, isSpace)

data Token
  = PercentageToken Double
  | WordToken String
  | PeriodToken
  deriving (Show, Eq)

type AST = String

type IR = String

-- Step 1: Lexical Analysis
lexicalAnalysis :: String -> [Token]
lexicalAnalysis "" = []
lexicalAnalysis input@(c : cs)
  | isAlpha c =
      let (word, rest) = span isAlpha input
       in WordToken word : lexicalAnalysis rest
  | isDigit c =
      let (intPart, afterInt) = span isDigit input
          (number, rest) = case afterInt of
            ('.' : cs') ->
              let (fracPart, afterFrac) = span isDigit cs'
               in (intPart ++ "." ++ fracPart, afterFrac)
            _ -> (intPart, afterInt)
       in case rest of
            ('%' : rest') -> PercentageToken (read number) : lexicalAnalysis rest'
            _ -> errorWithoutStackTrace ("Expected '%' after number: " ++ number)
  | isSpace c = lexicalAnalysis cs
  | otherwise = case c of
      '.' -> PeriodToken : lexicalAnalysis cs
      '%' -> errorWithoutStackTrace "Expected '%' after number. '%' found alone"
      _ -> errorWithoutStackTrace ("Unexpected character: " ++ [c])

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
