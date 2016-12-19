module Utils where
import Debug.Trace

traceIf :: Bool -> String -> a -> a
traceIf enable msg arg
  | enable = trace msg arg
  | otherwise = arg
