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
produce g s = produce' g $ newTape s

produce' :: (Eq a, Ord a, Show a) => Grammar a -> Tape a -> ErrorIO [a]
produce' g t
  | isAtEnd t = return []
  | otherwise = do
      (t', prod) <- mapErrorM $! produceOne g t
      prod' <- produce' g t'
      chosen <- randomElement $! prod
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
            t' <- (skipRight meta t >>= moveRight)
                  `amendE'` ("produceOne(isBreak) " ++ fragment)
            return (t', [[]])
        | isBlank meta x -> justCopy t
        | otherwise ->
            case lookupRule t g of
              Just (matched, rule) -> do
                productions <- applyRule rule t
                if null productions then justCopy t
                  else do
                    t' <- moveRightBy t matched
                          `amendE'` ("produceOne(result) " ++ fragment)
                    return (t', productions)
              Nothing -> justCopy t

justCopy :: Tape a -> GramError a
justCopy t = do
  t' <- moveRight t `amendE'` "justCopy"
  return (t', [[(head . tapeHead) $! t]])

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

lookupRule :: (Eq a, Ord a, Show a) => Tape a -> Grammar a -> Maybe ([a], LRule a)
lookupRule t (Grammar _ rules) = do
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
