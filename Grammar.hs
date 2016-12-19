module Grammar (
  Grammar(..),
  getMetagrammar,
  setMetagrammar,
  addRuleFromSpec,
  produceOne,
  produce
  )
where
import Utils
import Metagrammar
import Rule
import Data.List (isPrefixOf, sortBy)
import qualified Data.Map as Map

data Grammar a = Grammar (Metagrammar a) (Map.Map [a] (LRule a)) deriving (Show)

getMetagrammar :: Grammar a -> Metagrammar a
getMetagrammar (Grammar meta _) = meta

setMetagrammar :: Metagrammar a -> Grammar a -> Grammar a
setMetagrammar meta (Grammar _ rules) = Grammar meta rules

addRuleFromSpec :: (Ord a, Show a) => (RuleSpec a, [a]) -> Grammar a -> Grammar a
addRuleFromSpec newRule (Grammar meta rules) =
  let rule@(spec, production) = newRule
      pred = headCond spec
  in
    case Map.lookup pred rules of
      Nothing -> Grammar meta (Map.insert pred (makeRule spec production) rules)
      Just aRule ->
        Grammar meta $ Map.insert pred (addSuccessor rule aRule) rules
  
produce :: (Eq a, Ord a, Show a) => Grammar a -> Tape a -> Tape a -> Tape a
produce g tapeIn tapeOut
  | atEnd tapeIn = traceIf False ("atEnd " ++ show tapeOut) tapeOut
  | otherwise =
    let (nextIn, prod) = produceOne g tapeIn
        newOut = appendHead tapeOut $ head prod -- Just pick first possibility.
    in
      traceIf False ("produce " ++ show prod ++ ", nextIn = " ++ show nextIn ++ ", newOut = " ++
             show newOut) $
      produce g nextIn newOut
    
produceOne :: (Eq a, Ord a, Show a) => Grammar a -> Tape a -> (Tape a, [[a]])
produceOne g@(Grammar meta _) tapeIn =
  let justCopy' = (moveRight tapeIn, [[(head . tapeHead) tapeIn]])
      justCopy = traceIf False ("justCopy " ++ show justCopy') justCopy
  in
    case tapeHead tapeIn of
      [] -> (tapeIn, [[]])
      (x:_)
        | isSkipBalanced meta x -> traceIf False "foo" (moveRight $ skipRight meta tapeIn, [[]])
        | isBlank meta x -> traceIf False "isBlank" justCopy
        | otherwise ->
            case lookupRule tapeIn g of
              Just (matched, mrule) ->
                case traceIf False (show mrule) mrule of
                  Just rule ->
                    let productions = applyRule rule tapeIn in
                      if null productions then traceIf False "null productions" justCopy
                      else traceIf False ("returning " ++ show productions)
                      (moveRightBy tapeIn (length matched),
                        productions)
                  Nothing -> traceIf False "No spec match" justCopy
              Nothing -> traceIf False "No rule" justCopy

lookupRule :: (Eq a, Ord a) => Tape a -> Grammar a -> Maybe ([a], Maybe (LRule a))
lookupRule tape (Grammar _ rules) =
  case matchLongestPrefix tape $ Map.keys rules of
    Nothing -> Nothing
    Just matched -> Just (matched, Map.lookup matched rules)

matchLongestPrefix :: Eq a => Tape a -> [[a]] -> Maybe [a]
matchLongestPrefix tape prefixes =
  case
       filter (\a -> a `isPrefixOf` tapeHead tape) $
       sortBy (\a b -> length b `compare` length a) prefixes  -- TODO Cache this
  of
    [] -> Nothing
    x  -> Just $ last x
