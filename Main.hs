{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
module Main where
import Parse
import Grammar
import Options
import PlotTurtle
import Tape
import Utils
import Control.Monad
import Data.List
import System.Console.CmdLib
import System.Environment

data Mode = Test | Fractal | Exit deriving (Eq)

data Main = Main {
  fractal    :: Bool,
  input      :: String,
  iterations :: Int,
  test       :: Bool,
  output     :: String
  } deriving (Typeable, Data, Eq)

instance Attributes Main where          
  attributes _ = System.Console.CmdLib.group "Commands" [
      test %> [ Help "Examine the results of grammar productions." ],
      fractal %> [ Help "Produce a fractal from the input rules." ],
      iterations %> [ Help $ "Specifies how many iterations to run on the system. " ++
                      "This defaults to the number specified in the rules file.",
                      Short ['n'] ],
      input %> [ Help "The name of input rules file.",
                 Positional 0,
                 Required True],
      output %> [ Help "Specifies a file for output.",
                  Short ['o'] ] ]

instance RecordCommand Main where
  mode_summary _ = "Create a description of a Lindenmayer fractal for input to a rendering program."
  run' _ = putStrLn . head 
  
defaultOpts :: Main
defaultOpts = Main {
  fractal    = False,
  input      = "no input file specified",
  iterations = -1,
  test       = False,
  output     = "-"
  }
  
main :: IO ()
main = getArgs >>= executeR defaultOpts >>= \opts -> do
  result <- runExceptT
    (showResults opts (input opts)
    `catchError` \x -> liftIO (print ("*** Failed with " ++ x)))
  return ()

derive :: (Turt a, Eq b, Ord b, Show b) => LSystem a b -> [b] -> Int -> ErrorIO [b]
derive sys start 0 = return start
derive sys start n = do
  production <- produce (lGrammar sys) start
  derive sys production (n - 1)

growPlant :: (Turt a) => Main -> LSystem a Char -> ErrorIO String
growPlant opts sys = do
  count <- if iterations opts == -1 then
      mapErrorM $ getOption "iterate" 1 $ getOptions sys
    else
      return $ iterations opts
  derive sys (lAxiom sys) (trace ("count = " ++ show count) count)

showResults ::  Main -> String -> ExceptT String IO ()
showResults opts input = do
  mode <- liftIO $ mergeModeOpts opts
  text <- liftIO $ readFile input
  sys <- mapErrorM (parseRuleFile text)
  plant <- growPlant opts sys
  if mode == Test then
    liftIO $ putStrLn ("Final is " ++ plant)
  else do
    result <- liftIO $ runExceptT $ plotLSystem sys plant
    case result of
      Right a -> liftIO $ print a
      Left a -> liftIO $ print a

mergeModeOpts :: Main -> IO Mode
mergeModeOpts opts =
  case opts of
    _
      | fractal opts && test opts ->
          do
            putStrLn "\"--fractal\" and \"--test\" can not both be selected."
            return Exit
      | test opts -> return Test
      | otherwise -> return Fractal

