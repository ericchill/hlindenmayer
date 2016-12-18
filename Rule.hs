module Rule (
  LRule,
  makeRule,
  applyRule,
  addSuccessor,
  module RuleSpec,
  module Tape,
) where
import Prelude hiding (lookup)
import RuleSpec
import Tape
import Data.Map.Strict (Map, filterWithKey, findMax, insert, lookup, singleton)
import Data.String.Utils

data LRule a = LRule (Map (RuleSpec a) [[a]]) deriving (Eq, Show)

makeRule :: RuleSpec a -> [a] -> LRule a
makeRule spec production = LRule $ singleton spec [production]

applyRule :: Eq a => LRule a -> Tape a -> [[a]]
applyRule (LRule rules) tape =
  let matches = filterWithKey (\spec _ -> matchSpec spec tape) rules
  in
   snd $ findMax matches

addSuccessor :: (Eq a, Show a) => (RuleSpec a, [a]) -> LRule a -> LRule a
addSuccessor (spec, prod) (LRule rules) =
  case lookup spec rules of
    Just productions -> LRule $ insert spec (prod:productions) rules
    _ -> LRule $ singleton spec [prod]

