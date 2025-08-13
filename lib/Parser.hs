module Parser
  ( parser,
    makeComponent,
    Component (..),
    System (..),
  )
where

import Lexer (Token (..))

data System = System
  { systemComponents :: [Component]
  }
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

parser :: [Token] -> System
parser tokens = System {systemComponents = parseComponents tokens}

parseComponents :: [Token] -> [Component]
parseComponents [] = []
parseComponents tokens = case parseComponent tokens of
  Just (component, remaining) -> component : parseComponents remaining
  Nothing -> []

parseComponent :: [Token] -> Maybe (Component, [Token])
parseComponent tokens = do
  (name, rest1) <- parseName tokens
  rest2 <- expectWord "expects" rest1
  rest3 <- expectWord "an" rest2
  rest4 <- expectWord "uptime" rest3
  rest5 <- expectWord "of" rest4
  (uptime, rest6) <- parseUptime rest5
  rest7 <- expectWord "and" rest6
  rest8 <- expectWord "depends" rest7
  rest9 <- expectWord "on" rest8
  (dependencies, rest10) <- parseDependencies rest9
  let component =
        Component
          { name = name,
            uptimeExpectation = uptime,
            dependencies = dependencies
          }
  return (component, rest10)

parseName :: [Token] -> Maybe (String, [Token])
parseName (WordToken name : rest) = Just (name, rest)
parseName _ = Nothing

expectWord :: String -> [Token] -> Maybe [Token]
expectWord expected (WordToken actual : rest)
  | expected == actual = Just rest
  | otherwise = Nothing
expectWord _ _ = Nothing

parseUptime :: [Token] -> Maybe (Double, [Token])
parseUptime (PercentageToken uptime : rest) = Just (uptime, rest)
parseUptime _ = Nothing

parseDependencies :: [Token] -> Maybe ([String], [Token])
parseDependencies tokens = case parseName tokens of
  Nothing -> Just ([], tokens) -- "nothing" case
  Just (firstDep, rest) -> parseDependencyList [firstDep] rest
  where
    parseDependencyList deps (WordToken "and" : rest) = case parseName rest of
      Just (nextDep, remaining) -> parseDependencyList (deps ++ [nextDep]) remaining
      Nothing -> Just (deps, WordToken "and" : rest)
    parseDependencyList deps rest = Just (deps, rest)

takeUsedTokens :: [Token] -> [Token] -> [Token]
takeUsedTokens original remaining = take (length original - length remaining) original