module Main where

main :: IO ()
main = do
  putStrLn (greet "Matt")
  putStrLn (greet "World")
  printConfig
  
greet :: [Char] -> [Char]
greet name = "Hello " ++ name ++ "!"

printConfig = do
  contents <- readFile "stack.yaml"
  putStrLn contents
  
