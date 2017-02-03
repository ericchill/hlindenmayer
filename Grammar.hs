module Grammar (
  Grammar(..)
  , newGrammar
  , gSetIgnore
  , addRuleFromSpec
  , produceOne
  , produce
  )
where
import Error
import Math
import Metagrammar
import NumEval
import Rule
import RuleSpec
import Tape
import Utils
import Control.Arrow
import Data.Char (isSpace)
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

produce :: Bindings -> Grammar -> String -> ErrorIO String
produce b g s = produceRec b g (newTape s)

produceRec :: Bindings -> Grammar -> Tape -> ErrorIO String
produceRec b g t
  | isAtEnd t = return []
  | otherwise = do
      (t', choices) <- t `seq` mapErrorM $ produceOne b g t
      chosen <- randomElement choices
      prod' <- produceRec b g t'
      return $ chosen ++ prod'

produceOne :: Bindings -> Grammar -> Tape -> GramError
produceOne b g t
  | isAtEnd t      = return (t, [])
  | isBreak meta x = produceBreak t
  | isSpace x      = justCopy t
  | otherwise = do
      match <- lookupRule g t
      case match of
        Just (m, rule) -> do
          prods <- applyRule m rule t
          if Prelude.null prods then justCopy t
          else doProduction meta b (head prods) t
        Nothing -> justCopy t
  where meta = gMeta g
        x = tapeAtHead t

lookupRule :: Grammar -> Tape -> ErrorM (Maybe (TermMatch, LRule))
lookupRule (Grammar meta rules keys) t = do
  match <- matchLongestTerm meta keys t
  case match of
    Just m@(TermMatch spec _) ->
      return $ Just (m, fromJust $ Map.lookup spec rules)
    _ -> return Nothing

doProduction :: Metagrammar -> Bindings -> RuleApplication -> Tape -> GramError
doProduction meta bindings appl t =
  let matchedLen = termMatchLength $ cmPred $ raContext appl
      bindings' = addContextBindings (raContext appl) bindings
  in do
      t' <- moveRightBy matchedLen t
      metCond <- filterM (testCondition bindings') (raProds appl)
      if null metCond then justCopy t
      else do
        probs <- mapM (evalProbability bindings') metCond
        prodStrs <- mapM (renderProduction bindings') metCond
        return (t', zip probs prodStrs)

renderProduction :: Bindings -> Production -> ErrorM String
renderProduction b p =
  mapLeft $ foldM (\acc p -> do
            facStr <- evalFactor b p
            return $ acc ++ facStr)
  "" (pProduction p)

produceBreak :: Tape -> GramError
produceBreak t = do
  -- !!!! This is wrong
  t' <- skipRight t `amendE'` "produceBreak"
  moveRight t' `amendE'` "produceBreak"
  return (t', [])

justCopy :: Tape -> GramError
justCopy t = do
  t' <- moveRight t `amendE'` "justCopy"
  (arg, t'') <- copyArgument t'
  return (t'', [(1, tapeAtHead t : arg)])

copyArgument :: Tape -> ErrorM (String, Tape)
copyArgument t
  | isAtEnd t = return ([], t)
  | x == '('  = do
      (rest, t') <- skipAndCopy t `amendE'` "copyArgument"
      return (x : rest, t')
  | otherwise = return ([], t)
  where x = tapeAtHead t

gSetIgnore :: String -> Grammar -> Grammar
gSetIgnore x g = g { gMeta = mSetIgnore x $ gMeta g }

addRuleFromSpec :: RuleSpec -> Production -> Grammar -> Grammar
addRuleFromSpec spec prod g =
  let pred     = rsPred spec
      rules    = gRules g
      newRules =
        case Map.lookup pred rules of
          Nothing    -> Map.insert pred (makeRule spec prod) rules
          Just aRule -> Map.insert pred (addSuccessor spec prod aRule) rules
  in
    g { gRules            = newRules,
        gLengthSortedKeys = sortBy (compare `on` specTermLength) $
                            Map.keys newRules }
