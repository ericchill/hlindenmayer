module Error (
  ErrorIO,
  ErrorM,
  throwE',
  catchE',
  amendE',
  appendErrorT,
  mapErrorM,
  trace,
  module Control.Monad.Except,
  module Control.Monad.Identity
  )
  where
import Control.Monad.Except
import Control.Monad.Identity
import qualified Debug.Trace as Trace

type ErrorIO = ExceptT String IO

type ErrorM = ExceptT String Identity

throwE' :: (Monad m) => String -> ExceptT String m a
throwE' = throwError

catchE' :: (Monad m) =>
  ExceptT String m a -> (String -> ExceptT String m a) -> ExceptT String m a
catchE' = catchError

amendE' :: (Monad m) => ExceptT String m a -> String -> ExceptT String m a
amendE' m extra = m `catchError` \err -> throwError $ err ++ "; " ++ extra
  
appendErrorT :: String -> ErrorM a -> ErrorM a
appendErrorT a = withExceptT (++ a)

mapErrorM :: (Monad m) => ErrorM a -> ExceptT String m a
mapErrorM = mapExceptT (return . runIdentity)

trace :: String -> a -> a
trace msg arg = if traceOn then Trace.trace msg arg else arg

traceOn :: Bool
traceOn = False
