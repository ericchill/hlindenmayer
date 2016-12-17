module Grammar where
import Rule
import RuleSpec
import Tape
import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.String.Utils

metagrammar :: String -> Metagrammar Char
metagrammar toIgnore = Metagrammar
  (\a -> a `elem` "([")
  (\a -> a `elem` ")]")
  (\a b -> case a of
      '(' -> b == ')'
      '[' -> b == ']')
  (\a -> isSpace a)
  (\a -> a `elem` toIgnore)
  (\a -> a == '*')
  (\a -> a == '%')
  "()[]*"

data Grammar a = Grammar (Metagrammar a) (Map.Map [a] (LRule a))

newGrammar :: Metagrammar Char -> Grammar Char
newGrammar meta = Grammar meta Map.empty

addRule :: String -> Grammar Char -> Grammar Char
addRule ruleStr g@(Grammar meta _) = addRuleFromSpec (parseRule meta ruleStr) g

addRuleFromSpec :: (RuleSpec Char, String) -> Grammar Char -> Grammar Char
addRuleFromSpec newRule g@(Grammar meta rules) =
  let rule@(s@(RuleSpec _ p _ _), production) = newRule
  in
    case Map.lookup p rules of
      Nothing -> Grammar meta (Map.insert p (makeRule s production) rules)
      Just aRule ->
        Grammar meta (Map.insert p (addSuccessor rule aRule) rules)

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
                Nothing -> error "This shouldn't happen."
            Nothing -> tapeOut

lookupRule :: (Eq a, Ord a) => Tape a -> Grammar a -> Maybe ([a], Maybe (LRule a))
lookupRule tape (Grammar _ rules) =
  case matchLongestPrefix tape $ Map.keys rules of
    Nothing -> Nothing
    Just pred -> Just (pred, Map.lookup pred rules)

matchLongestPrefix :: Eq a => Tape a -> [[a]] -> Maybe [a]
matchLongestPrefix tape prefixes =
  -- haystack should be sorted already.
  -- sortBy (\a b -> (length b) `compare` (length a)) $
  case
       filter (\a -> a `isPrefixOf` (tapeHead tape)) prefixes
  of
    [] -> Nothing
    x  -> Just $ (head . reverse) x

