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

data RuleSpec = RuleSpec {
  rsMeta  :: Metagrammar,
  rsLeft  :: String,
  rsPred  :: String,
  rsRight :: String
  } deriving (Eq, Show)

matchSpec :: RuleSpec -> Tape -> ErrorM Bool
matchSpec spec@(RuleSpec meta l p r) t =
    if isPrefixOfIgnoring meta p $ tapeHead t then
      (lcondiff meta l t &&&& rcondiff meta r t)
        `amendE'` ("matchSpec " ++ show l ++ " < " ++
                   show p ++
                   " > " ++ show r ++
                  "; t @ " ++ show (tapeHead t))
    else return False

matchSpecExact :: RuleSpec -> RuleSpec -> Bool
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

isPrefixOfIgnoring :: Metagrammar -> String -> String -> Bool
isPrefixOfIgnoring meta pfx@(p:ps) (s:ss)
  | isIgnored meta s = isPrefixOfIgnoring meta pfx ss
  | p == s           = isPrefixOfIgnoring meta ps ss
  | otherwise        = False
isPrefixOfIgnoring _ [] _  = True
isPrefixOfIgnoring _ _ _  = False

