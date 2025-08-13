module SemanticAnalyzer (semanticAnalysis) where

import Parser (Component (..), System (..))

semanticAnalysis :: System -> Bool
semanticAnalysis system = all validComponent (systemComponents system)
  where
    validComponent component =
      let componentDeps = dependencies component
       in all (`elem` map name (systemComponents system)) componentDeps
