module RuleSpec (
  RuleSpec(..),
  matchSpec,
  matchSpecExact,
  Metagrammar,
  Tape
  ) where
import Utils
import Metagrammar
import Tape
import Data.List (isPrefixOf)

data RuleSpec a = RuleSpec
  { rsMeta :: Metagrammar a,
    leftCond :: [a],  -- Left condition must be reversed.
    headCond :: [a],
    rightCond :: [a]
  } deriving (Eq, Show)

instance (Eq a, Show a) => Ord (RuleSpec a) where
  a@(RuleSpec ma la pa ra) <= b@(RuleSpec mb lb pb rb)
    | pa /= pb = error $
      "RuleSpecs incomparable without matching predecessor: " ++ show a ++ ", " ++ show b
    | rsSig ma /= rsSig mb =
      error "RuleSpecs incomparable because of mismatched metagramma."
    | otherwise = (lb `accepts` la) && (rb `accepts` ra)
    where accepts a b
            | isWild ma $ head a = True
            | otherwise          = a `isPrefixOf` b
  
matchSpec :: (Eq a, Show a) => RuleSpec a -> Tape a -> Bool
matchSpec spec@(RuleSpec meta l p r) tape =
    traceIf False ("checking prefix " ++ (show p) ++ " at " ++ (show $ tapeHead tape)) $
    isPrefixOfIgnoring meta p (tapeHead tape) &&
    traceIf False "checking lcondiff" (wild l || lcondiff meta l tape) &&
    traceIf False "checking rcondiff" (wild r || rcondiff meta r tape) &&
    traceIf False "specs match" True
    where wild s =
            isWild meta . head $
            traceIf False ("matching " ++ show spec ++ " w? " ++ (show s)) s

matchSpecExact :: Eq a => RuleSpec a -> RuleSpec a -> Bool
matchSpecExact a b
  | predIn /= predSpec = False
  | if null la then notWild lb else la /= lb = False
  | if null ra then notWild rb else ra /= rb = False
  | rsSig metaA /= rsSig metaB = False
  | otherwise = True
  where
    (RuleSpec metaA la predIn ra) = a
    (RuleSpec metaB lb predSpec rb) = b
    notWild = not . isWild metaA . head

isPrefixOfIgnoring :: (Eq a) => Metagrammar a -> [a] -> [a] -> Bool
isPrefixOfIgnoring meta pfx@(p:ps) (s:ss)
  | isIgnored meta s = isPrefixOfIgnoring meta pfx ss
  | p == s           = isPrefixOfIgnoring meta ps ss
  | otherwise        = False
isPrefixOfIgnoring _ [] _  = True
isPrefixOfIgnoring _ _ _  = False

