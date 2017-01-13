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

type GramError a = ErrorM (Tape a, [[a]])

data Grammar a = Grammar {
  gMeta             :: Metagrammar a,
  gRules            :: Map.Map [a] (LRule a),
  gLengthSortedKeys :: [[a]]
  } deriving (Show)

newGrammar :: Metagrammar a -> Grammar a
newGrammar meta = Grammar meta Map.empty []

produce :: (Eq a, Ord a, Show a) => Grammar a -> [a] -> ErrorIO [a]
produce g s = produceRec g (newTape s)

produceRec :: (Eq a, Ord a, Show a) => Grammar a -> Tape a -> ErrorIO [a]
produceRec g t
  | isAtEnd t = return []
  | otherwise = do
      (t', prod) <- t `seq` mapErrorM $ produceOne g t
      chosen <- randomElement prod
      prod' <- produceRec g t'
      return $ chosen ++ prod'

produceOne :: (Eq a, Ord a, Show a) => Grammar a -> Tape a -> GramError a
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

doProduction :: (Eq a, Ord a, Show a) =>
  Metagrammar a -> [a] -> [[a]] -> Tape a -> GramError a
doProduction meta matched prods t = do
  t' <- moveRightBy (length matched) t `amendE'` "doProduction"
  (_, t'') <- copyArgument meta t' -- skip over old argument
  return (t'', prods)

produceBreak :: (Eq a, Ord a, Show a) => Grammar a -> Tape a -> GramError a
produceBreak g t = do
  t' <- skipRight (gMeta g) t `amendE'` "produceBreak(1)"
  moveRight t' `amendE'` "produceBreak(2)"
  return (t', [[]])

justCopy :: (Eq a, Show a) => Metagrammar a -> Tape a -> GramError a
justCopy meta t = do
  let x = (head . tapeHead) t
  t' <- moveRight t `amendE'` "justCopy"
  (arg, t'') <- copyArgument meta t'
  return (t'', [x : arg])

copyArgument :: (Eq a, Show a) => Metagrammar a -> Tape a -> ErrorM ([a], Tape a)
copyArgument meta t
  | isAtEnd t = return ([], t)  -- isOpenBracket may fail if we don't do this first
  | isFuncArg meta $ (head . tapeHead) t = do
      (rest, t') <- skipAndCopy meta t `amendE'` "copyArgument"
      return ((head . tapeHead) t : rest, t')
  | otherwise = return ([], t)

gSetIgnore :: (Eq a) => [a] -> Grammar a -> Grammar a
gSetIgnore x g = g { gMeta = mSetIgnore x $ gMeta g }

addRuleFromSpec :: (Ord a, Show a) => RuleSpec a -> [a] -> Grammar a -> Grammar a
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

lookupRule :: (Eq a, Ord a, Show a) => Grammar a -> Tape a -> Maybe ([a], LRule a)
lookupRule (Grammar _ rules keys) t = do
  matched <- matchLongestPrefix (tapeHead t) keys
  return (matched, fromJust $ Map.lookup matched rules)
