module Rule (
  LRule,
  makeRule,
  applyRule,
  addSuccessor,
  module RuleSpec,
  module Tape,
) where
import Utils
import RuleSpec
import Tape
import Data.AssocList
import Data.String.Utils

data LRule = LRule (AssocList RuleSpec [String]) deriving (Eq, Show)

makeRule :: RuleSpec -> String -> LRule
makeRule spec production = LRule [(spec, [production])]

applyRule :: LRule -> Tape -> ErrorM [String]
applyRule (LRule rules) t = do
  matches <- filterM (\(spec, _) -> matchSpec spec t) rules
  if null matches then return []
    else return $ (snd . head) matches

addSuccessor :: RuleSpec -> String -> LRule -> LRule
addSuccessor spec prod rule@(LRule rules) =
  let productions = lookup1 spec rules
  in
    LRule $ addEntry spec (prod:productions) rules

