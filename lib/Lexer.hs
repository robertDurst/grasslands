module Lexer
  ( lexicalAnalysis,
    Token (..),
  )
where

import Control.Applicative ((<|>))
import Data.Char (isAlpha)
import Text.Parsec hiding ((<|>))
import Text.Parsec.String

data Token
  = PercentageToken Double
  | WordToken String
  | PeriodToken
  deriving (Show, Eq)

lexicalAnalysis :: String -> [Token]
lexicalAnalysis input =
  case parse tokenizer "" input of
    Left err -> error $ "Lexical analysis failed: " ++ show err
    Right tokens -> tokens

tokenizer :: Parser [Token]
tokenizer = do
  skipWhitespace
  tokens <- many (try (parseToken <* skipWhitespace))
  eof
  return tokens
  where
    skipWhitespace = skipMany (space <|> newline)

parseToken :: Parser Token
parseToken = percentageToken <|> wordToken <|> periodToken

percentageToken :: Parser Token
percentageToken = do
  num <- number
  _ <- char '%'
  return $ PercentageToken num

wordToken :: Parser Token
wordToken = do
  word <- many1 (satisfy isAlpha)
  return $ WordToken word

periodToken :: Parser Token
periodToken = do
  _ <- char '.'
  return PeriodToken

number :: Parser Double
number = do
  intPart <- many1 digit
  fracPart <- optionMaybe (char '.' >> many1 digit)
  case fracPart of
    Nothing -> return $ read intPart
    Just frac -> return $ read (intPart ++ "." ++ frac)
