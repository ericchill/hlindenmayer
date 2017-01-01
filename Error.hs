module Error (
  ErrorIO,
  ErrorM,
  appendError,
  appendErrorT,
  mapErrorM,
  BoolMonad,
  module Control.Monad.Except,
  module Control.Monad.Identity
  )
  where
import Control.Monad.Except
import Control.Monad.Identity

--have things use this!
type ErrorIO = ExceptT String IO

type ErrorM = ExceptT String Identity

type BoolMonad = ErrorM Bool

-- For use with catchError
appendError :: String -> String -> ErrorM a
appendError extra err = throwError $ err ++ "; " ++ extra

appendErrorT :: String -> ErrorM a -> ErrorM a
appendErrorT a = withExceptT (++ a)

mapErrorM :: Monad m => ErrorM a -> ExceptT String m a
mapErrorM = mapExceptT (return . runIdentity)
