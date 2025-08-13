module System
  ( System (..),
  )
where

import Component (Component (..))

data System = System
  {systemComponents :: [Component]}
  deriving (Show, Eq)