module Grammar (
  Grammar(..),
  getMetagrammar,
  setMetagrammar,
  addRuleFromSpec,
  produce
  )
where
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
  
produce :: (Eq a, Ord a) => Grammar a -> Tape a -> Tape a -> Tape a
produce g@(Grammar meta _) tapeIn tapeOut =
  case tapeHead tapeIn of
    []      -> tapeOut
    (x:_)
      | isSkipBalanced meta x -> produce g (moveRight $ skipRight meta tapeIn) tapeOut
      | isBlank meta x -> produce g (moveRight tapeIn) tapeOut
      | otherwise ->
          case lookupRule tapeIn g of
            Just (matched, mrule) ->
              case mrule of
                Just rule ->
                  let newOut = applyRule rule tapeIn tapeOut
                  in
                    produce g (moveRightBy tapeIn (length matched)) newOut
                Nothing -> produce g (moveRight tapeIn) tapeOut
            Nothing -> tapeOut

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
