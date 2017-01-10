{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
module Main where
import Parse
import Grammar
import Options
import POVTurtle
import Tape
import Utils
import Control.Monad
import Data.List
import System.Console.CmdLib
import System.Environment
import System.Exit

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
  runExceptT
    (showResults opts (input opts)
    `catchE'` \x -> do
        liftIO $ print ("*** Failed with " ++ x)
        liftIO exitFailure)
  return ()

derive :: (Turt a, Eq b, Ord b, Show b) => Mode -> LSystem a b -> [b] -> Int -> ErrorIO [b]
derive mode sys start 0 = return start
derive mode sys start n = do
  production <- produce (lGrammar sys) start
  derive mode sys ((
    if mode == Test then trace ("step " ++ show n ++ ": " ++ show production)
    else id) production)
    (n - 1)

growPlant :: (Turt a) => Mode -> Main -> LSystem a Char -> ErrorIO String
growPlant mode opts sys = do
  count <- if iterations opts == -1 then
      mapErrorM $ getOption "iterate" 1 $ getOptions sys
    else
      return $ iterations opts
  derive mode sys (lAxiom sys) count

showResults ::  Main -> String -> ExceptT String IO ()
showResults opts input = do
  text <- liftIO $ readFile input
  sys <- mapErrorM (parseRuleFile text)
  mode <- liftIO $ mergeModeOpts opts
  plant <- growPlant mode opts sys
  if mode == Test then
    liftIO $ putStrLn ("Final is " ++ plant)
  else
    povLSystem sys plant

mergeModeOpts :: Main -> IO Mode
mergeModeOpts opts =
  case opts of
    _ | fractal opts && test opts ->
          do
            putStrLn "\"--fractal\" and \"--test\" can not both be selected."
            return Exit
      | test opts -> return Test
      | otherwise -> return Fractal

