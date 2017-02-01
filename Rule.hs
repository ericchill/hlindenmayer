module Rule (
    LRule
  , Production(..)
  , ProdFactor(..)
  , RuleApplication(..)
  , makeRule
  , applyRule
  , testCondition
  , evalProbability
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

type RuleItem = (RuleSpec, [Production])
  
data ProdFactor = ProdFactor {
    pfName  :: String
  , pfFuncs :: [Evaluator]
  } 

data RuleApplication  = RuleApplication {
    raContext :: ContextMatch
  , raProds   :: [Production]
  }
  
instance Show Production where
  show p = "Production " ++ show (pProduction p)

instance Show ProdFactor where
  show = pfName

makeRule :: RuleSpec -> Production -> LRule
makeRule spec production = LRule [(spec, [production])]

applyRule :: TermMatch -> LRule -> Tape -> ErrorM [RuleApplication]
applyRule pred (LRule rules) = applyRuleRec pred rules

applyRuleRec :: TermMatch -> [RuleItem] -> Tape -> ErrorM [RuleApplication]
applyRuleRec _ [] _ = return []
applyRuleRec pred ((spec, prods):xs) t = do
  context <- matchContext pred spec t
  rest <- applyRuleRec pred xs t
  return $ case context of
    Just ctx -> RuleApplication ctx prods : rest
    Nothing  -> rest

testCondition :: Bindings -> Production -> ErrorM Bool
testCondition bindings prod =
  case pCond prod of
    Just p -> do
      x <- mapLeft $ runEvaluator p bindings
      return (x /= 0)
    _ -> return False

evalProbability :: Bindings -> Production -> ErrorM Double
evalProbability bindings prod =
  case pProbability prod of
    Just p -> mapErrorM $ mapLeft $ runEvaluator p bindings
    _      -> return 1.0

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

addSuccessor :: RuleSpec -> Production -> LRule -> LRule
addSuccessor spec prod rule@(LRule rules) =
  let productions = lookup1 spec rules
      newList = addEntry spec (prod : productions) rules
  in
    LRule $ sortBy (\(a,_) (b,_) -> wildness a `compare` wildness b) newList
