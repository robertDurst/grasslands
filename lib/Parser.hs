module Parser
  ( parser,
    makeComponent,
    Component (..),
    System (..),
  )
where

import Lexer (Token (..))

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

-- Entry point
parser :: [Token] -> System
parser = System . go
  where
    go [] = []
    go ts = case parseComponent ts of
      Just (c, rest) -> c : go rest
      Nothing -> []

-- Component parser
parseComponent :: [Token] -> Maybe (Component, [Token])
parseComponent tokens = do
  (n, t1) <- satisfyWord tokens
  t2 <- expectWords ["expects", "an", "uptime", "of"] t1
  (uptime, t3) <- satisfyPercentage t2
  t4 <- expectWords ["and", "depends", "on"] t3
  (deps, t5) <- parseDependencies t4
  rest <- expectPeriod t5
  pure (Component n uptime deps, rest)

-- Dependency list parser
parseDependencies :: [Token] -> Maybe ([String], [Token])
parseDependencies tokens = case satisfyWord tokens of
  Just ("nothing", rest) -> Just ([], rest) -- Special case: "nothing" means no dependencies
  _ -> go [] tokens
  where
    go acc ts = case satisfyWord ts of
      Just (dep, WordToken "and" : rest) -> go (acc ++ [dep]) rest
      Just (dep, rest) -> Just (acc ++ [dep], rest)
      Nothing -> Just (acc, ts)

-- Generic token matchers
satisfyWord :: [Token] -> Maybe (String, [Token])
satisfyWord (WordToken w : rest) = Just (w, rest)
satisfyWord _ = Nothing

satisfyPercentage :: [Token] -> Maybe (Double, [Token])
satisfyPercentage (PercentageToken p : rest) = Just (p, rest)
satisfyPercentage _ = Nothing

expectWord :: String -> [Token] -> Maybe [Token]
expectWord expected (WordToken actual : rest)
  | expected == actual = Just rest
  | otherwise = Nothing
expectWord _ _ = Nothing

expectWords :: [String] -> [Token] -> Maybe [Token]
expectWords [] ts = Just ts
expectWords (w : ws) ts = do
  rest <- expectWord w ts
  expectWords ws rest

expectPeriod :: [Token] -> Maybe [Token]
expectPeriod (SeperatorToken : rest) = Just rest
expectPeriod _ = Nothing
