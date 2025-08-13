module Component
  ( Component (..),
    makeComponent,
  )
where

data Component = Component
  { name :: String,
    uptimeExpectation :: Double,
    dependencies :: [String]
  }
  deriving (Show, Eq)

makeComponent :: String -> Double -> [String] -> Component
makeComponent name uptimeExpectation dependencies =
  Component {name = name, uptimeExpectation = uptimeExpectation, dependencies = dependencies}