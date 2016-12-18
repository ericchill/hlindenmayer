module Main where
import Grammar
import Parse
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  s <- readFile $ head args
  let sys = parseRuleFile s in do
    putStrLn $ "Axiom is " ++ show (axiom sys) ++ "."
    putStrLn $ "Grammar is " ++ show (grammar sys)
    
