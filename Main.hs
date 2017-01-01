module Main where
import Parse
import Grammar
import Utils
import PlotTurtle
import Tape
import Control.Monad
import Data.List
import System.Environment

type RunMonad = ExceptT String IO

derive :: (Turt a, Eq b, Ord b, Show b) => LSystem a b -> [b] -> ErrorM [b]
derive sys axiom =
  do
    foo <- produce (lGrammar sys) axiom
    return $ traceIf False ("derive result is " ++ show foo) foo

deriveN :: (Turt a, Eq b, Ord b, Show b) => LSystem a b -> Int -> ErrorM [b]
deriveN sys n = foldM (\i _ -> derive sys i) (lAxiom sys) [1..n]

growPlant :: Turt a => LSystem a Char -> ErrorM String
growPlant sys =
  do
    count <- getOption sys "iterate" 1
    deriveN sys count

showResults :: String -> String -> ExceptT String IO ()
showResults opt arg =
  do
    text <- liftIO $ readFile arg
    sys <- mapErrorM (parseRuleFile text)
    plant <- mapErrorM (growPlant sys)
    if "-g" == arg then
      liftIO $ putStrLn ("Final is " ++ plant)
      else
      do
        plotLSystem sys plant
        return ()


main :: IO ()
main = do
  args <- getArgs
  let args' = if "-g" == head args then
                tail args
              else
                args
  runExceptT (showResults (head args) $ head args')
  return ()
