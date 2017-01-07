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

data LRule a = LRule (AssocList (RuleSpec a) [[a]]) deriving (Eq, Show)

makeRule :: RuleSpec a -> [a] -> LRule a
makeRule spec production = LRule [(spec, [production])]

applyRule :: (Eq a, Show a) => LRule a -> Tape a -> ErrorM [[a]]
applyRule (LRule rules) t = do
  matches <- filterM (\(spec, _) -> matchSpec spec t) rules
  if null $! matches then return []
    else return $ snd $ head matches

addSuccessor :: (Eq a, Show a) => RuleSpec a -> [a] -> LRule a -> LRule a
addSuccessor spec prod rule@(LRule rules) =
  let productions = lookup1 spec rules
  in
    LRule $ addEntry spec (prod:productions) rules

