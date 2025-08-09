-- | A minimal Haskell library module
module Grasslands
  ( greet,
    add,
  )
where

-- | A simple greeting function
greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

-- | A simple addition function
add :: Int -> Int -> Int
add x y = x + y
