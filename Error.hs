module Error (
  ErrorIO,
  ErrorM,
  throwE,
  amendE,
  appendErrorT,
  mapErrorM,
  BoolMonad,
  trace',
  module Control.Monad.Except,
  module Control.Monad.Identity
  )
  where
import Control.Monad.Except
import Control.Monad.Identity
--import Control.Monad.Trans.Except as ExceptT (throwE, catchE)
import Debug.Trace

--have things use this!
type ErrorIO = ExceptT String IO

type ErrorM = ExceptT String Identity

type BoolMonad = ErrorM Bool

throwE :: (Monad m) => String -> ExceptT String m a
throwE = throwError

amendE :: (Monad m) => ExceptT String m a -> String -> ExceptT String m a
amendE m extra =
  m `catchError` appendError (trace' extra extra)
  
appendErrorT :: String -> ErrorM a -> ErrorM a
appendErrorT a = withExceptT (++ a)

mapErrorM :: (Monad m) => ErrorM a -> ExceptT String m a
mapErrorM = mapExceptT (return . runIdentity)

-- For use with catchError
appendError :: (Monad m) => String -> String -> ExceptT String m a
appendError err extra = throwError $ err ++ "; " ++ extra

trace' :: String -> a -> a
trace' msg arg = if traceOn then trace msg arg else arg

traceOn :: Bool
traceOn = True
