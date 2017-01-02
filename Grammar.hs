module Grammar (
  Grammar(..),
  newGrammar,
  getMetagrammar,
  setMetagrammar,
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
import Data.List (isPrefixOf, sortBy)
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
produce' g tape
  | isAtEnd tape = return []
  | otherwise = do
      (tape', prod) <- mapErrorM $ produceOne g tape
      prod' <- produce' g tape'
      chosen <- if 1 < length prod
        then randomElement prod
        else (return . head) prod
      return $ chosen ++ prod'
    
produceOne :: (Eq a, Ord a, Show a) => Grammar a -> Tape a -> GramError a
produceOne g@(Grammar meta _) tape =
  let
    fragment = "\"" ++ show (take 10 $ tapeHead tape) ++ "\""
    justCopy = do
      tape' <- moveRight tape
               `amendE` ("produceOne(justCopy) " ++ fragment)
      return (tape', [[(head . tapeHead) tape]])
  in
    case tapeHead tape of
      [] -> return (tape, [[]])
      (x:_)
        | isSkipBalanced meta x -> do
            tape' <- (skipRight meta tape >>= moveRight)
                     `amendE` ("produceOne(isSkipBalanced) " ++ fragment)
            return (tape', [[]])
        | isBlank meta x -> justCopy
        | otherwise ->
            case lookupRule tape g of
              Just (matched, mrule) ->
                case mrule of
                  Just rule -> do
                    productions <- applyRule rule tape
                    if null productions then justCopy
                    else do
                      tape' <- moveRightBy tape matched
                               `amendE` ("produceOne(result) " ++ fragment)
                      return (tape', productions)
                  Nothing -> justCopy
              Nothing -> justCopy

getMetagrammar :: Grammar a -> Metagrammar a
getMetagrammar  = gMeta

setMetagrammar :: Metagrammar a -> Grammar a -> Grammar a
setMetagrammar meta g = g { gMeta = meta }

gSetIgnore :: (Eq a) => [a] -> Grammar a -> Grammar a
gSetIgnore x g = g { gMeta = mSetIgnore x $ gMeta g }

addRuleFromSpec :: (Ord a, Show a) =>
                   (RuleSpec a, [a]) -> Grammar a -> Grammar a
addRuleFromSpec rule@(spec, production) (Grammar meta rules) =
  let pred = headCond spec
  in
    case Map.lookup pred rules of
      Nothing -> Grammar meta (Map.insert pred (makeRule spec production) rules)
      Just aRule ->
        Grammar meta $ Map.insert pred (addSuccessor rule aRule) rules

lookupRule :: (Eq a, Ord a, Show a) => Tape a -> Grammar a ->
              Maybe ([a], Maybe (LRule a))
lookupRule tape (Grammar _ rules) =
  case matchLongestPrefix tape $ Map.keys rules of
    Just matched -> Just (matched, Map.lookup matched rules)
    Nothing -> Nothing

matchLongestPrefix :: (Eq a) => Tape a -> [[a]] -> Maybe [a]
matchLongestPrefix tape prefixes =
  case
       filter (\a -> a `isPrefixOf` tapeHead tape) $
       -- TODO Cache lengths
       sortBy (\a b -> length b `compare` length a) prefixes
  of
    x@(_:_) -> Just $ last x
    [] -> Nothing
