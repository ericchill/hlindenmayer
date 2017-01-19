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
produceOne g t
  | isAtEnd t      = return (t, [[]])
  | isBreak meta x = produceBreak g t
  | isBlank meta x = justCopy meta t
  | otherwise =
      case lookupRule g t of
        Just (matched, rule) -> do
          prods <- applyRule rule t
          if Prelude.null prods then justCopy meta t
          else doProduction meta matched prods t
        Nothing -> justCopy meta t
  where meta = gMeta g
        x = tapeAtHead t

doProduction :: Metagrammar -> String -> [String] -> Tape -> GramError
doProduction meta matched prods t = do
  t' <- moveRightBy (length matched) t `amendE'` "doProduction"
  (_, t'') <- copyArgument meta t' -- skip over old argument
  return (t'', prods)

produceBreak :: Grammar -> Tape -> GramError
produceBreak g t = do
  t' <- skipRight (gMeta g) t `amendE'` "produceBreak"
  moveRight t' `amendE'` "produceBreak"
  return (t', [[]])

justCopy :: Metagrammar -> Tape -> GramError
justCopy meta t = do
  t' <- moveRight t `amendE'` "justCopy"
  (arg, t'') <- copyArgument meta t'
  return (t'', [tapeAtHead t : arg])

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

addRuleFromSpec :: RuleSpec -> String -> Grammar -> Grammar
addRuleFromSpec spec prod g =
  let pred = rsPred spec
      rules = gRules g
      newRules =
        case Map.lookup pred rules of
          Nothing    -> Map.insert pred (makeRule spec prod) rules
          Just aRule -> Map.insert pred (addSuccessor spec prod aRule) rules
  in
    g { gRules = newRules,
        gLengthSortedKeys = sortBy (compare `on` length) $
                            Map.keys newRules }

lookupRule :: Grammar -> Tape -> Maybe (String, LRule)
lookupRule (Grammar _ rules keys) t = do
  matched <- matchLongestPrefix (tapeHead t) keys
  return (matched, fromJust $ Map.lookup matched rules)
