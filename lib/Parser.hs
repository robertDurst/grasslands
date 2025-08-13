module Parser
  ( parser,
    makeComponent,
    Component (..),
    System (..),
  )
where

import Control.Applicative ((<|>))
import Lexer (Token (..))
import Text.Parsec hiding ((<|>))

data System = System
  {systemComponents :: [Component]}
  deriving (Show, Eq)

data Component = Component
  { name :: String,
    uptimeExpectation :: Double,
    dependencies :: [String]
  }
  deriving (Show, Eq)

makeComponent :: String -> Double -> [String] -> Component
makeComponent name uptimeExpectation dependencies =
  Component {name = name, uptimeExpectation = uptimeExpectation, dependencies = dependencies}

type TokenParser = Parsec [Token] ()

parser :: [Token] -> System
parser tokens =
  case parse systemParser "" tokens of
    Left err -> error $ "Parser failed: " ++ show err
    Right system -> system

systemParser :: TokenParser System
systemParser = do
  components <- many componentParser
  eof
  return $ System components

componentParser :: TokenParser Component
componentParser = do
  name <- wordToken
  expectWords ["expects", "an", "uptime", "of"]
  uptime <- percentageToken
  expectWords ["and", "depends", "on"]
  deps <- dependenciesParser
  seperatorToken
  return $ Component name uptime deps

dependenciesParser :: TokenParser [String]
dependenciesParser = nothingParser <|> dependencyListParser
  where
    nothingParser = do
      expectWord "nothing"
      return []

    dependencyListParser = do
      first <- wordToken
      rest <- many (expectWord "and" >> wordToken)
      return (first : rest)

wordToken :: TokenParser String
wordToken = tokenPrim show updatePos testWord
  where
    testWord (WordToken w) = Just w
    testWord _ = Nothing
    updatePos pos _ _ = incSourceColumn pos 1

percentageToken :: TokenParser Double
percentageToken = tokenPrim show updatePos testPercentage
  where
    testPercentage (PercentageToken p) = Just p
    testPercentage _ = Nothing
    updatePos pos _ _ = incSourceColumn pos 1

seperatorToken :: TokenParser ()
seperatorToken = tokenPrim show updatePos testSeperator
  where
    testSeperator SeperatorToken = Just ()
    testSeperator _ = Nothing
    updatePos pos _ _ = incSourceColumn pos 1

expectWord :: String -> TokenParser ()
expectWord expected = tokenPrim show updatePos testExpected
  where
    testExpected (WordToken actual) | actual == expected = Just ()
    testExpected _ = Nothing
    updatePos pos _ _ = incSourceColumn pos 1

expectWords :: [String] -> TokenParser ()
expectWords words = mapM_ expectWord words
