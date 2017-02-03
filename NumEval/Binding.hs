module NumEval.Binding (
    EvalError
  , Evaluator(..)
  , StrEvaluator(..)
  , Bindings
  , defaultBindings
  , addToBindings
  , bindScalar
  , getScalar
  , getFunction
) where
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map.Strict as Map

type EvalError = Either String

newtype Evaluator = Evaluator {
  runEvaluator :: Bindings -> EvalError Double
  }

newtype StrEvaluator = StrEvaluator {
  runStrEvaluator :: Bindings -> EvalError String
  }

data BoundValue = BoundNum Evaluator
                | BoundNumFunc Evaluator [String]
                | BoundStr StrEvaluator
                | BoundStrFunc StrEvaluator [String]

type Bindings = Map.Map String BoundValue

boundScalar :: Double -> BoundValue
boundScalar x = BoundNum $ Evaluator (\_ -> return x)

boundString :: String -> BoundValue
boundString x = BoundStr $ StrEvaluator (\_ -> return x)

defaultBindings :: Bindings
defaultBindings =
  foldl (\b (k, v) -> Map.insert k (BoundNum $ Evaluator $ \_ -> return v) b)
  Map.empty
  [("false", 0), ("true", 1), ("pi", pi)]

addToBindings :: [(String, Double)] -> Bindings -> Bindings
addToBindings new bindings =
  foldr (\(k, v) b -> bindScalar k (Evaluator (\_ -> return v)) b) bindings new

bindScalar :: String -> Evaluator -> Bindings -> Bindings
bindScalar name eval = Map.insert name (BoundNum eval)

getScalar :: String -> Bindings -> EvalError Evaluator
getScalar name bindings = do
  bound <- getBinding name bindings
  case bound of
    BoundNum eval -> return eval
    _ -> throwError $ show name ++ " is not bound to a scalar."
    
getFunction :: String -> Bindings -> EvalError (Evaluator, [String])
getFunction name bindings = do
  bound <- getBinding name bindings
  case bound of
    BoundNumFunc bound params -> return (bound, params)
    _ -> throwError $ show name ++ " is not bound to a function."

getString :: String -> Bindings -> EvalError StrEvaluator
getString name bindings = do
  bound <- getBinding name bindings
  case bound of
    BoundStr eval -> return eval
    _ -> throwError $ show name ++ " is not bound to a string."
  
getBinding :: String -> Bindings -> EvalError BoundValue
getBinding name bindings =
  case Map.lookup name bindings of
    Just x  -> return x
    Nothing -> throwError $
      show name ++ " is not bound. Keys are " ++ (show $ Map.keys bindings)
