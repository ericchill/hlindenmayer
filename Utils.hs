module Utils where
import Debug.Trace

traceIf :: Bool -> String -> a -> a
traceIf enable msg arg = if enable then trace msg arg else arg
