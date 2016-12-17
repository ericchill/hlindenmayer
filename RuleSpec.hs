module RuleSpec where
import Tape
import Data.List

parseRuleSpec :: Metagrammar Char -> [Char] -> RuleSpec Char
-- TODO Implement this
parseRuleSpec meta specStr =
  RuleSpec "" "" "" meta

data Metagrammar a = Metagrammar {
  isOpenBracket  :: a -> Bool,
  isCloseBracket :: a -> Bool,
  closesBracket  :: a -> a -> Bool,
  isBlank        :: a -> Bool,
  isIgnored      :: a -> Bool,
  isWild         :: a -> Bool,
  isSkipBalanced :: a -> Bool,
  rsSig          :: [a]
  }

instance Eq a => Eq (Metagrammar a) where
  a == b = (rsSig a) == rsSig b

instance Show a => Show (Metagrammar a) where
  show a = "(Metagrammar \"" ++ (show $ rsSig a) ++ "\")"

data RuleSpec a = RuleSpec
  { leftCond :: [a],
    headCond :: [a],
    rightCond :: [a],
    rsMeta :: Metagrammar a
  } deriving (Eq, Show)

instance Eq a => Ord (RuleSpec a) where
  (RuleSpec la pa ra ma) <= (RuleSpec lb pb rb mb)
    | pa /= pb       = error "RuleSpecs incomparable without matching predecessor."
    | (rsSig ma) /= (rsSig mb) =
      error "RuleSpecs incomparable because of mismatched metagramma."
    | otherwise = (lb `accepts` la) && (rb `accepts` ra)
    where accepts a b
            | (isWild ma) $ head a = True
            | otherwise            = a `isPrefixOf` b
  
matchSpec :: Eq a => RuleSpec a -> Tape a -> Bool
matchSpec (RuleSpec l p r meta) tape =
    isPrefixOfIgnoring meta p (tapeHead tape) &&
    lcondiff meta (reverse l) tape &&
    rcondiff meta r tape

matchSpecExact :: Eq a => RuleSpec a -> RuleSpec a -> Bool
matchSpecExact a b
  | p /= pred = False
  | (case null lc of
      True  -> (not . (isWild metaA)) l
      False -> lc /= left) = False
  | (case null rc of
      True  -> (not . (isWild metaA)) r
      False -> rc /= right) = False
  | (rsSig metaA) /= (rsSig metaB) = False
  | otherwise = True
  where
    (RuleSpec lc p rc metaA) = a
    (RuleSpec left@(l:ls) pred right@(r:rs) metaB) = b
    

-- Call this with the match string reversed
lcondiff :: Eq a => Metagrammar a -> [a] -> Tape a -> Bool
lcondiff _ [] _ = True
lcondiff meta s@(x:xs) tape
  | ((isIgnored meta) t) || (isOpenBracket meta) t =
      lcondiff meta s (moveLeft tape)
  | (isCloseBracket meta) t =
      lcondiff meta s (skipLeft meta tape)
  | t == x =
      True
  | otherwise =
      lcondiff meta xs (moveLeft tape)
  where t = head $ tapeHead tape

rcondiff :: Eq a => Metagrammar a -> [a] -> Tape a -> Bool
rcondiff _ [] _ = True
rcondiff meta s@(x:xs) tape
  | (isIgnored meta) t =
      rcondiff meta s (moveRight tape)
  | (isOpenBracket meta) x =
      rcondiff meta s (skipRight meta tape)
  | t == x =
      True
  | otherwise =
      rcondiff meta xs (moveRight tape)
  where t = head $ tapeHead tape
  
-- For a string starting with a balanced delimiter, skip past the closing item.
skipRight :: Eq a => Metagrammar a -> Tape a -> Tape a
skipRight _ (Tape _ []) = error "Already at end in skipRight"
skipRight meta foo@(Tape _ (x:_)) = skipRightRec meta foo [x]

skipRightRec :: Eq a => Metagrammar a -> Tape a -> [a] -> Tape a
skipRightRec _ xs [] = xs
skipRightRec _ (Tape _ []) _ = error $ "skipRight: Missing closing delimiter."
skipRightRec meta tape delimStack@(d:ds)
  | ((isOpenBracket meta) d) && ((closesBracket meta) x d) =
      skipRightRec meta tape' ds
  | (isOpenBracket meta) x =
      skipRightRec meta tape' (x:ds)
  | otherwise =
    skipRightRec meta tape' delimStack
  where
    tape' = moveRight tape
    x = (head . tapeHead) tape

-- Leave head right before closing item
skipLeft :: Eq a => Metagrammar a -> Tape a -> Tape a
skipLeft _ (Tape _ []) = error "Already at end in skipLeft"
skipLeft meta tape = skipLeftRec meta tape [(head . tapeHead) tape]

skipLeftRec :: Eq a => Metagrammar a -> Tape a -> [a] -> Tape a
skipLeftRec _ xs [] = xs
skipLeftRec _ (Tape _ []) _ = error $ "skipLeft: Missing opening delimiter."
skipLeftRec meta tape delimStack@(d:ds)
  | ((isCloseBracket meta) d) && (closesBracket meta) d x =
      skipLeftRec meta tape' ds
  | (isCloseBracket meta) x =
      skipLeftRec meta tape' (x:ds)
  | otherwise =
      skipLeftRec meta tape' delimStack
  where tape' = moveLeft tape
        x = (head . tapeHead) tape

isPrefixOfIgnoring :: (Eq a) => Metagrammar a -> [a] -> [a] -> Bool
isPrefixOfIgnoring _ _ [] = False
isPrefixOfIgnoring _ [] _ = True
isPrefixOfIgnoring meta pfx@(p:ps) (s:ss)
  | (isIgnored meta) s = isPrefixOfIgnoring meta pfx ss
  | p == s             = isPrefixOfIgnoring meta ps ss
  | otherwise          = False
  
    
