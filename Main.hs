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
import qualified Debug.Trace as Trace
import System.Console.CmdLib
import System.Environment
import System.Exit

data Mode = GrammarOpt | Fractal | Exit deriving (Eq)

data Main = Main {
  fractal    :: Bool,
  grammar    :: Bool,
  input      :: String,
  iterations :: Int,
  output     :: String
  } deriving (Typeable, Data, Eq)

instance Attributes Main where          
  attributes _ = System.Console.CmdLib.group "Commands" [
      fractal %> [ Help "Produce a fractal from the input rules." ],
      grammar %> [ Help "Examine the results of grammar productions." ],
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
  grammar    = False,
  input      = "no input file specified",
  iterations = -1,
  output     = "-"
  }
  
main :: IO ()
main = getArgs >>= executeR defaultOpts >>= \opts -> do
  runExceptT
    (showResults opts (input opts)
    `catchE'` \x -> do
        liftIO $ putStrLn ("*** Failed with " ++ x)
        liftIO exitFailure)
  return ()

derive :: (Turt a) => Mode -> LSystem a -> String -> Int -> ErrorIO String
derive mode sys start 0 = return start
derive mode sys start n = do
  production <- produce (lGrammar sys) start `amendE'` ("derive " ++ show start)
  derive mode sys ((
    if mode == GrammarOpt then
      trace ("step " ++ show n ++ ": " ++ show production)
    else id) production)
    (n - 1)

growPlant :: (Turt a) => Mode -> Main -> LSystem a -> ErrorIO String
growPlant mode opts sys = do
  count <- if iterations opts == -1 then
      mapErrorM $ getIntOption "iterate" 1 $ getOptions sys
    else
      return $ iterations opts
  derive mode sys (lAxiom sys) count `amendE'` ("glowPlant " ++ show sys)

showResults ::  Main -> String -> ExceptT String IO ()
showResults opts input = do
  text <- liftIO $ readFile input
  sys <- mapErrorM $ parseRuleFile text
  mode <- liftIO $ mergeModeOpts opts
  plant <- growPlant mode opts sys
  if mode == GrammarOpt then
    liftIO $ putStrLn ("Final is " ++ show plant)
  else
    povLSystem sys plant

mergeModeOpts :: Main -> IO Mode
mergeModeOpts opts =
  case opts of
    _ | fractal opts && grammar opts -> do
          putStrLn "\"--fractal\" and \"--grammar\" can not both be selected."
          return Exit
      | grammar opts -> return GrammarOpt
      | otherwise -> return Fractal

