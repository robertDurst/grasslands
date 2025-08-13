module Lexer
  ( lexicalAnalysis,
    Token (..),
  )
where

import Data.Char (isAlpha, isDigit, isSpace)

data Token
  = PercentageToken Double
  | WordToken String
  | PeriodToken
  deriving (Show, Eq)

-- Step 1: Lexical Analysis
lexicalAnalysis :: String -> [Token]
lexicalAnalysis "" = []
lexicalAnalysis input@(c : cs)
  | isAlpha c =
      let (word, rest) = lexWord input
       in WordToken word : lexicalAnalysis rest
  | isDigit c =
      let (intPart, afterInt) = lexInt input
          (number, rest) = case afterInt of
            ('.' : cs') ->
              let (fracPart, afterFrac) = lexInt cs'
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

lexWord :: String -> (String, String)
lexWord = span isAlpha

lexInt :: String -> (String, String)
lexInt = span isDigit
