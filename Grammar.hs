module Grammar (
  Grammar(..),
  newGrammar,
  gSetIgnore,
  addRuleFromSpec,
  produceOne,
  produce,
  module Metagrammar
  )
where
import Utils
import Math
import Metagrammar
import NumEval.Binding
import Rule
import RuleSpec
import Tape
import Control.Arrow
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (sortBy, intercalate)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map

type GramError = ErrorM (Tape, [(Double, String)])

data Grammar = Grammar {
    gMeta             :: Metagrammar
  , gRules            :: Map.Map SpecTerm LRule
  , gLengthSortedKeys :: [SpecTerm]
  }

instance Show Grammar where
  show g = "Grammar"
  
newGrammar :: Metagrammar -> Grammar
newGrammar meta = Grammar meta Map.empty []

produce :: Grammar -> String -> ErrorIO String
produce g s = produceRec g (newTape s)

produceRec :: Grammar -> Tape -> ErrorIO String
produceRec g t
  | isAtEnd t = return []
  | otherwise = do
      (t', choices) <- t `seq` mapErrorM $ produceOne g t
      chosen <- randomElement choices
      prod' <- produceRec g t'
      return $ chosen ++ prod'

produceOne :: Grammar -> Tape -> GramError
produceOne g t
  | isAtEnd t      = return (t, [])
  | isBreak meta x = produceBreak g t
  | isBlank meta x = justCopy meta t
  | otherwise = do
      match <- lookupRule g t
      case match of
        Just (m, rule) -> do
          prods <- applyRule rule t
          if Prelude.null prods then justCopy meta t
          else doProduction meta m prods t
        Nothing -> justCopy meta t
  where meta = gMeta g
        x = tapeAtHead t

lookupRule :: Grammar -> Tape -> ErrorM (Maybe (TermMatch, LRule))
lookupRule (Grammar meta rules keys) t = do
  match <- matchLongestTerm meta keys t
  case match of
    Just m@(TermMatch spec _) ->
      return $ Just (m, fromJust $ Map.lookup spec rules)
    _ -> return Nothing

doProduction :: Metagrammar -> TermMatch -> [Production] -> Tape -> GramError
doProduction meta (TermMatch spec matched) prods t = do
  t' <- foldM (\t fm -> moveRightBy (fmLength fm) t) t matched
  let paramNames =
        foldl (\names fm -> names ++ sfParams (fmFactor fm)) [] matched
      argVals = foldl (\vals fm -> vals ++ fmArgs fm) [] matched
      bindings = foldl (\binds (k, v) ->
                   bindScalar k (Evaluator (\_ -> return v)) binds) Map.empty $
        zip paramNames argVals
    in do
      -- filter productions by condition
      metCond <- filterM (\p ->
                   case pCond p of
                     Just p -> do
                       x <- mapLeft $ runEvaluator p bindings
                       return (x /= 0)
                     Nothing -> return True) prods
      if null metCond then justCopy meta t
      else do
        probs <- mapM (\p ->
                       case pProbability p of
                         Just p -> mapErrorM $ mapLeft $ runEvaluator p bindings
                         Nothing -> return 1.0) metCond
        prodStrs <- mapM (renderProduction bindings) metCond
        -- evaluate productions
        return (t', zip probs prodStrs)

renderProduction :: Bindings -> Production -> ErrorM String
renderProduction b p =
  foldM (\acc p -> do
            facStr <- renderFactor b p
            return $ acc ++ facStr) "" (pProduction p)

renderFactor :: Bindings -> ProdFactor -> ErrorM String
renderFactor b f = do
  argVals <- mapM (\e -> mapErrorM $ mapLeft $ runEvaluator e b) (pfFuncs f)
  let argStrs = map showFloat argVals
      allArgs = intercalate "," argStrs
    in
    if null argVals then return $ pfName f
    else return $ pfName f ++ "(" ++ allArgs ++ ")"

produceBreak :: Grammar -> Tape -> GramError
produceBreak g t = do
  t' <- skipRight (gMeta g) t `amendE'` "produceBreak"
  moveRight t' `amendE'` "produceBreak"
  return (t', [])

justCopy :: Metagrammar -> Tape -> GramError
justCopy meta t = do
  t' <- moveRight t `amendE'` "justCopy"
  (arg, t'') <- copyArgument meta t'
  return (t'', [(1, tapeAtHead t : arg)])

copyArgument :: Metagrammar -> Tape -> ErrorM (String, Tape)
copyArgument meta t
  | isAtEnd t        = return ([], t)
  | isFuncArg meta x = do
      (rest, t') <- skipAndCopy meta t `amendE'` "copyArgument"
      return (x : rest, t')
  | otherwise = return ([], t)
  where x = tapeAtHead t

gSetIgnore :: String -> Grammar -> Grammar
gSetIgnore x g = g { gMeta = mSetIgnore x $ gMeta g }

addRuleFromSpec :: RuleSpec -> Production -> Grammar -> Grammar
addRuleFromSpec spec prod g =
  let pred = rsPred spec
      rules = gRules g
      newRules =
        case Map.lookup pred rules of
          Nothing    -> Map.insert pred (makeRule spec prod) rules
          Just aRule -> Map.insert pred (addSuccessor spec prod aRule) rules
  in
    g { gRules = newRules,
        gLengthSortedKeys = sortBy (compare `on` specTermLength) $
                            Map.keys newRules }
