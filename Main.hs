module Main where
import Grammar
import Parse
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  s <- readFile $ head args
  let sys = parseRuleFile s in
    print $ grammar sys
    
