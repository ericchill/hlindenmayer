module NumEval (
    eval
  , runable
  , parseExpr
  , module NumEval.Binding
  ) where
import NumEval.Binding
import NumEval.Parser
import NumEval.Primitive
import NumEval.Translate
import qualified Data.Map.Strict as Map

eval :: String -> String -> EvalError Double
eval ctx s = do
  e <- runable ctx s
  runEvaluator e defaultBindings

runable :: String -> String -> EvalError Evaluator
runable ctx s =
  case parseExpr ctx s of
    Left err   -> throwError $ show err
    Right expr -> translate expr

