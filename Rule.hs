module Rule (
  LRule,
  makeRule,
  applyRule,
  addSuccessor,
  module RuleSpec,
  module Tape,
) where
import Utils
import Prelude hiding (lookup)
import RuleSpec
import Tape
import Data.AssocList
--import Data.Map.Strict (Map, filterWithKey, findMax, insert, lookup, singleton)
import Data.String.Utils

data LRule a = LRule (AssocList (RuleSpec a) [[a]]) deriving (Eq, Show)

makeRule :: RuleSpec a -> [a] -> LRule a
makeRule spec production = LRule [(spec, [production])]

applyRule :: (Eq a, Show a) => LRule a -> Tape a -> [[a]]
applyRule (LRule rules) tape =
  let matches = filter (\(spec, _) -> matchSpec spec tape) rules in
    if null matches then []
    else snd $ head matches

addSuccessor :: (Eq a, Show a) => (RuleSpec a, [a]) -> LRule a -> LRule a
addSuccessor (spec, prod) rule@(LRule rules) =
  let productions = lookup1 spec rules
  in
    LRule $ addEntry spec (prod:productions) rules

