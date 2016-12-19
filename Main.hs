module Main where
import Utils
import Grammar
import Parse
import Tape
import Control.Monad
import Data.List
import System.Environment

newtype ProcessPair a = ProcessPair { runProcess :: a }

instance Functor ProcessPair where
  fmap = liftM

instance Applicative ProcessPair where
  pure = ProcessPair
  (<*>) = ap

instance Monad ProcessPair where
  return = pure
  (>>) = (*>)
  ProcessPair a >>= f = f a

run :: (Eq a, Ord a, Show a) => LSystem a -> Tape a -> Tape a -> Tape a
run sys tIn tOut =
  let foo = rewind $ produce (grammar sys) tIn tOut
  in traceIf True ("run result is " ++ show (tapeHead foo)) foo

run5 :: (Eq a, Ord a, Show a) => LSystem a -> Tape a
run5 sys =
  let tapeIn = newTape $ axiom sys in
    foldl' (\i _ -> run sys i $ newTape []) tapeIn [1..5]
  
main :: IO ()
main = do
  args <- getArgs
  s <- readFile $ head args
  let sys = parseRuleFile s
    in do
    putStrLn $ "Grammar is " ++ show (grammar sys)
    putStrLn $ "Axiom is " ++ show (axiom sys)
    putStrLn $ tapeHead $ run5 sys
