module RuleSpec (
  RuleSpec(..),
  matchSpec,
  matchSpecExact,
  Metagrammar,
  Tape
  ) where
import Metagrammar
import Tape
import Data.List (isPrefixOf)


data RuleSpec a = RuleSpec
  { rsMeta :: Metagrammar a,
    leftCond :: [a],
    headCond :: [a],
    rightCond :: [a]
  } deriving (Eq, Show)

instance Eq a => Ord (RuleSpec a) where
  (RuleSpec ma la pa ra) <= (RuleSpec mb lb pb rb)
    | pa /= pb       = error "RuleSpecs incomparable without matching predecessor."
    | (rsSig ma) /= (rsSig mb) =
      error "RuleSpecs incomparable because of mismatched metagramma."
    | otherwise = (lb `accepts` la) && (rb `accepts` ra)
    where accepts a b
            | (isWild ma) $ head a = True
            | otherwise            = a `isPrefixOf` b
  
matchSpec :: Eq a => RuleSpec a -> Tape a -> Bool
matchSpec (RuleSpec meta l p r) tape =
    isPrefixOfIgnoring meta p (tapeHead tape) &&
    lcondiff meta (reverse l) tape &&
    rcondiff meta r tape

matchSpecExact :: Eq a => RuleSpec a -> RuleSpec a -> Bool
matchSpecExact a b
  | predIn /= predSpec = False
  | (case null lc of
      True  -> (not . (isWild metaA)) l
      False -> lc /= left) = False
  | (case null rc of
      True  -> (not . (isWild metaA)) r
      False -> rc /= right) = False
  | (rsSig metaA) /= (rsSig metaB) = False
  | otherwise = True
  where
    (RuleSpec metaA lc predIn rc) = a
    (RuleSpec metaB left@(l:_) predSpec right@(r:_)) = b

isPrefixOfIgnoring :: (Eq a) => Metagrammar a -> [a] -> [a] -> Bool
isPrefixOfIgnoring _ _ [] = False
isPrefixOfIgnoring _ [] _ = True
isPrefixOfIgnoring meta pfx@(p:ps) (s:ss)
  | (isIgnored meta) s = isPrefixOfIgnoring meta pfx ss
  | p == s             = isPrefixOfIgnoring meta ps ss
  | otherwise          = False
  
    
