module Main where
import System.Environment

main :: IO ()
main = do putStrLn "Enter the expression (no parentheses, sorry): "
          e <- getLine
          putStrLn $ show $ parse $ words e

parse :: [String] -> Int
parse [x] = read x
parse exp = let 
    (x:y:xs) = exp
    a = read x
    b = parse xs
    result = case y of
      "+" -> a + b
      "-" -> a - b
      "*" -> a * b
      "/" -> a `quot` b
    in result
