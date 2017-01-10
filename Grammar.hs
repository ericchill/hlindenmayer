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
import Data.List (isPrefixOf, sortBy)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map

type GramError a = ErrorM (Tape a, [[a]])

data Grammar a = Grammar {
  gMeta  :: Metagrammar a,
  gRules :: Map.Map [a] (LRule a)
  } deriving (Show)

newGrammar :: Metagrammar a -> Grammar a
newGrammar meta = Grammar meta Map.empty

produce :: (Eq a, Ord a, Show a) => Grammar a -> [a] -> ErrorIO [a]
produce g s = produceRec g $ newTape s

produceRec :: (Eq a, Ord a, Show a) => Grammar a -> Tape a -> ErrorIO [a]
produceRec g t
  | isAtEnd t = return []
  | otherwise = do
      (t', prod) <- mapErrorM $! produceOne g t
      chosen <- randomElement prod
      prod' <- produceRec g t'
      return $ chosen ++ prod'
    
produceOne :: (Eq a, Ord a, Show a) => Grammar a -> Tape a -> GramError a
produceOne g t =
  let fragment = "\"" ++ show (take 10 $ tapeHead t) ++ "\""
      meta = gMeta g
  in
    case tapeHead t of
      [] -> return (t, [[]])
      (x:_)
        | isBreak meta x -> do
            t' <- skipRight meta t `amendE'` ("produceOne(0) " ++ fragment)
            moveRight t' `amendE'` ("produceOne(1) " ++ fragment)
            return (t', [[]])
        | isBlank meta x -> justCopy meta t
        | otherwise ->
            case lookupRule g t of
              Just (matched, rule) -> do
                productions <- applyRule rule t
                if null productions then justCopy meta t
                  else do
                    t' <- moveRightBy t matched
                          `amendE'` ("produceOne(result) " ++ fragment)
                    (_, t'') <- copyArgument meta t' -- skip over old argument
                    return (t'', productions)
              Nothing -> justCopy meta t

justCopy :: (Eq a, Show a) => Metagrammar a -> Tape a -> GramError a
justCopy meta t = do
  let x = (head . tapeHead) t
  t' <- moveRight t `amendE'` "justCopy(1)"
  (arg, t'') <- copyArgument meta t'
  return (t'', [x : arg])

copyArgument :: (Eq a, Show a) => Metagrammar a -> Tape a -> ErrorM ([a], Tape a)
copyArgument meta t
  | isAtEnd t = return ([], t)  -- isOpenBracket may fail if we don't do this first
  | isOpenBracket meta $ (head . tapeHead) t = do
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
    case Map.lookup pred rules of
      Nothing -> g { gRules = Map.insert pred (makeRule spec production) rules }
      Just aRule ->
        g { gRules = Map.insert pred (addSuccessor spec production aRule) rules }

lookupRule :: (Eq a, Ord a, Show a) => Grammar a -> Tape a -> Maybe ([a], LRule a)
lookupRule (Grammar _ rules) t = do
  matched <- matchLongestPrefix t $ Map.keys rules
  return (matched, fromJust $ Map.lookup matched rules)

matchLongestPrefix :: (Eq a) => Tape a -> [[a]] -> Maybe [a]
matchLongestPrefix t prefixes =
  case
       filter (\a -> a `isPrefixOf` tapeHead t) $
       sortBy (compare `on` length) prefixes
  of
    [] -> Nothing
    x  -> Just $ head x
