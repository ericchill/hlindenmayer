module Main where
import Grammar
import Parse
import Tape
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  s <- readFile $ head args
  let sys = parseRuleFile s
      tapeIn = newTape $ axiom sys
      tapeOut = newTape []
    in do
      putStrLn $ "Axiom is " ++ show (axiom sys) ++ "."
      let t' = produce (grammar sys) tapeIn tapeOut in
        putStrLn $ (reverse . leftPart) t'
        
