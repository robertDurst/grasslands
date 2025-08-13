module SemanticAnalyzer (semanticAnalysis) where

import Parser (Component (..), System (..))

semanticAnalysis :: System -> Bool
semanticAnalysis system = all validComponent (systemComponents system)
  where
    validComponent component =
      all (`elem` map name (systemComponents system)) componentDeps
      where
        componentDeps = dependencies component
