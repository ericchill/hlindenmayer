module Main where
import Parse
import Grammar
import Utils
import PlotTurtle
import Tape
import Control.Monad
import Data.List
import System.Environment

run :: (Turt a, Eq b, Ord b, Show b) => LSystem a b -> [b] -> [b]
run sys axiom =
  let foo = produce (lGrammar sys) axiom
  in traceIf True ("run result is " ++ show foo) foo

runN :: (Turt a, Eq b, Ord b, Show b) => LSystem a b -> Int -> [b]
runN sys n = foldl' (\i _ -> run sys i) (lAxiom sys) [1..n]
  
main :: IO ()
main = do
  args <- getArgs
  s <- readFile $ head args
  case parseRuleFile s :: Either String (LSystem PlotTurtle Char) of
    Right sys ->
      let plant = runN sys $ getIntOption sys "iterate" 1
          in
        do
          putStrLn ("Grammar is " ++ show (lGrammar sys))
          putStrLn ("Axiom is " ++ show (lAxiom sys))
          print plant
          plotLSystem sys plant
          return ()
    Left err -> print err
      
