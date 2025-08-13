module SemanticAnalyzer (semanticAnalysis) where

import Component (Component (..))
import System (System (..))

semanticAnalysis :: System -> Bool
semanticAnalysis system = all validComponent (systemComponents system)
  where
    validComponent component =
      all (`elem` map name (systemComponents system)) componentDeps
      where
        componentDeps = dependencies component
