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
import Metagrammar
import Rule
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map

type GramError = ErrorM (Tape, [String])

data Grammar = Grammar {
  gMeta             :: Metagrammar,
  gRules            :: Map.Map String LRule,
  gLengthSortedKeys :: [String]
  } deriving (Show)

newGrammar :: Metagrammar -> Grammar
newGrammar meta = Grammar meta Map.empty []

produce :: Grammar -> String -> ErrorIO String
produce g s = produceRec g (newTape s)

produceRec :: Grammar -> Tape -> ErrorIO String
produceRec g t
  | isAtEnd t = return []
  | otherwise = do
      (t', prod) <- t `seq` mapErrorM $ produceOne g t
      chosen <- randomElement prod
      prod' <- produceRec g t'
      return $ chosen ++ prod'

produceOne :: Grammar -> Tape -> GramError
produceOne g t =
  let meta = gMeta g in
    case tapeHead t of
      [] -> return (t, [[]])
      (x:_)
        | isBreak meta x -> produceBreak g t
        | isBlank meta x -> justCopy meta t
        | otherwise ->
            case lookupRule g t of
              Just (matched, rule) -> do
                prods <- applyRule rule t
                if null prods then justCopy meta t
                else doProduction meta matched prods t
              Nothing -> justCopy meta t

doProduction :: Metagrammar -> String -> [String] -> Tape -> GramError
doProduction meta matched prods t = do
  t' <- moveRightBy (length matched) t `amendE'` "doProduction"
  (_, t'') <- copyArgument meta t' -- skip over old argument
  return (t'', prods)

produceBreak :: Grammar -> Tape -> GramError
produceBreak g t = do
  t' <- skipRight (gMeta g) t `amendE'` "produceBreak(1)"
  moveRight t' `amendE'` "produceBreak(2)"
  return (t', [[]])

justCopy :: Metagrammar -> Tape -> GramError
justCopy meta t = do
  let x = (head . tapeHead) t
  t' <- moveRight t `amendE'` "justCopy"
  (arg, t'') <- copyArgument meta t'
  return (t'', [x : arg])

copyArgument :: Metagrammar -> Tape -> ErrorM (String, Tape)
copyArgument meta t
  | isAtEnd t = return ([], t)
  | isFuncArg meta $ (head . tapeHead) t = do
      (rest, t') <- skipAndCopy meta t `amendE'` "copyArgument"
      return ((head . tapeHead) t : rest, t')
  | otherwise = return ([], t)

gSetIgnore :: String -> Grammar -> Grammar
gSetIgnore x g = g { gMeta = mSetIgnore x $ gMeta g }

addRuleFromSpec :: RuleSpec -> String -> Grammar -> Grammar
addRuleFromSpec spec production g =
  let pred = rsPred spec
      rules = gRules g
  in
    let newRules = case Map.lookup pred rules of
          Nothing    -> Map.insert pred (makeRule spec production) rules
          Just aRule -> Map.insert pred (addSuccessor spec production aRule) rules
    in
      g { gRules = newRules,
          gLengthSortedKeys = sortBy (compare `on` length) $ Map.keys newRules }

lookupRule :: Grammar -> Tape -> Maybe (String, LRule)
lookupRule (Grammar _ rules keys) t = do
  matched <- matchLongestPrefix (tapeHead t) keys
  return (matched, fromJust $ Map.lookup matched rules)
