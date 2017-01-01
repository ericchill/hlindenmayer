module RuleSpec (
  RuleSpec(..),
  matchSpec,
  matchSpecExact,
  Metagrammar,
  Tape,
  ) where
import Metagrammar
import Tape
import Utils
import Data.List (isPrefixOf)

data RuleSpec a = RuleSpec {
  rsMeta :: Metagrammar a,
  leftCond :: [a],  -- Left condition must be reversed.
  headCond :: [a],
  rightCond :: [a]
  } deriving (Show)

-- RuleSpecs are only partially ordered, and therefor unsuitable for storing in Maps.
instance (Eq a, Show a) => Eq (RuleSpec a) where
  a@(RuleSpec ma la pa ra) == b@(RuleSpec mb lb pb rb)
    | pa /= pb             = False
    | ma /= mb             = False
    | lb == la && rb == ra = True
    | otherwise            = False

matchSpec :: (Eq a, Show a) => RuleSpec a -> Tape a -> BoolMonad
matchSpec spec@(RuleSpec meta l p r) tape =
    pure (isPrefixOfIgnoring meta p $ tapeHead tape) &&&&
    (if wild l then return True else lcondiff meta l tape) &&&&
    (if wild r then return True else rcondiff meta r tape) `catchError`
    appendError ("matchSpec " ++ show l ++ " < " ++ show p ++ " > " ++ show r)
    where wild = isWild meta . head

matchSpecExact :: Eq a => RuleSpec a -> RuleSpec a -> Bool
matchSpecExact a b
  | pa /= pb = False
  | if null la then notWild lb else la /= lb = False
  | if null ra then notWild rb else ra /= rb = False
  | ma /= mb = False
  | otherwise = True
  where
    (RuleSpec ma la pa ra) = a
    (RuleSpec mb lb pb rb) = b
    notWild = not . isWild ma . head

isPrefixOfIgnoring :: (Eq a) => Metagrammar a -> [a] -> [a] -> Bool
isPrefixOfIgnoring meta pfx@(p:ps) (s:ss)
  | isIgnored meta s = isPrefixOfIgnoring meta pfx ss
  | p == s           = isPrefixOfIgnoring meta ps ss
  | otherwise        = False
isPrefixOfIgnoring _ [] _  = True
isPrefixOfIgnoring _ _ _  = False

