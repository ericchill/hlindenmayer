module Rule (
    LRule
  , Production(..)
  , ProdFactor(..)
  , makeRule
  , applyRule
  , evalProduction
  , addSuccessor
) where
import Error
import Math
import NumEval.Binding
import RuleSpec
import Tape
import Utils
import Data.AssocList (AssocList, addEntry, lookup1)
import Data.List (intercalate, sortBy)

data LRule = LRule (AssocList RuleSpec [Production]) deriving (Show)

data Production = Production {
    pCond        :: Maybe Evaluator
  , pProduction  :: [ProdFactor]
  , pProbability :: Maybe Evaluator
  }
  
data ProdFactor = ProdFactor {
    pfName  :: String
  , pfFuncs :: [Evaluator]
  } 

instance Show Production where
  show p = "Production " ++ show (pProduction p)

instance Show ProdFactor where
  show = pfName

makeRule :: RuleSpec -> Production -> LRule
makeRule spec production = LRule [(spec, [production])]

applyRule :: LRule -> Tape -> ErrorM [Production]
applyRule (LRule rules) t = do
  matches <- filterM (\(spec, _) -> matchSpec spec t) rules
  if Error.trace ("\napply matches = " ++ show matches) $ null matches then
    return []
    else return $ (snd . head) matches

addSuccessor :: RuleSpec -> Production -> LRule -> LRule
addSuccessor spec prod rule@(LRule rules) =
  let productions = lookup1 spec rules
      newList = addEntry spec (prod : productions) rules
  in
    LRule $ sortBy (\(a,_) (b,_) -> wildness a `compare` wildness b) newList

evalProduction :: Bindings -> Production -> EvalError (Double, String)
evalProduction bindings (Production _ terms prob) = do
  strs <- mapM (evalFactor bindings) terms
  case prob of
    Just p -> do
      probX <- runEvaluator p bindings
      return (probX, concat strs)
    Nothing -> return (1.0, concat strs)

evalFactor :: Bindings -> ProdFactor -> EvalError String
evalFactor bindings (ProdFactor name args) =
  if null args then return name
  else do
    vals <- mapM (`runEvaluator` bindings) args
    let strs = map showFloat vals
      in
      return $ name ++ "(" ++ intercalate "," strs ++ ")"
