module Metagrammar (
  Metagrammar(..),
  mSetIgnore,
  testMeta,
  lcondiff,
  rcondiff,
  skipLeft,
  skipRight,
  Tape
    )
where
import Utils
import Tape
import Control.Applicative
import Control.Monad

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

instance (Eq a) => Eq (Metagrammar a) where
  a == b = rsSig a == rsSig b

instance (Show a) => Show (Metagrammar a) where
  show a = "(Metagrammar " ++ show (rsSig a) ++ ")"

mSetIgnore :: (Eq a) => [a] -> Metagrammar a -> Metagrammar a
mSetIgnore a m = m { isIgnored = (`elem` a) }

testMeta :: a -> Metagrammar a
testMeta forType =
  let cf = const False
      cf2 = const (const False)
  in
  Metagrammar {
    isOpenBracket  = cf,
    isCloseBracket = cf,
    closesBracket  = cf2,
    isBlank        = cf,
    isIgnored      = cf,
    isWild         = cf,
    isSkipBalanced = cf,
    rsSig          = [forType]
    }

-- Check left context
lcondiff :: (Eq a, Show a) => Metagrammar a -> [a] -> Tape a -> BoolMonad
lcondiff _ [] _ = return True
lcondiff meta s@(x:xs) tape =
  caseM [
      (isIgnored meta <$> h     , diffNext),
      (isOpenBracket meta <$> h , diffNext),
      (isCloseBracket meta <$> h, skipLeft meta tape >>= lcondiff meta s),
      ((x /=) <$> h             , return False)]
      diffNext
  where  t' = moveLeft tape `amendE` "lcondiff"
         h = head . tapeHead <$> t'
         diffNext = join $ lcondiff meta xs <$> t'

-- Leave head right before closing item
skipLeft :: (Eq a) => Metagrammar a -> Tape a -> TapeMonad a
skipLeft meta t
  | isAtStart t = throwE "Already at end in skipLeft"
  | otherwise   = skipLeftRec meta [(head . tapeHead) t] t

skipLeftRec :: (Eq a) => Metagrammar a -> [a] -> Tape a -> TapeMonad a
skipLeftRec _ [] xs = return xs
skipLeftRec meta delimStack@(d:ds) tape
  | isAtStart tape = throwE "skipLeft: Missing opening delimiter."
  | closesBracket meta d x   = tape' >>= skipLeftRec meta ds
  | isCloseBracket meta x    = tape' >>= skipLeftRec meta (x:ds)
  | otherwise                = tape' >>= skipLeftRec meta delimStack
  where tape' = moveLeft tape `amendE` "skipLeftRec"
        x = (head . tapeHead) tape

-- Check right context
rcondiff :: (Eq a, Show a) => Metagrammar a -> [a] -> Tape a -> BoolMonad
rcondiff _ [] _ = return True
rcondiff meta s@(x:xs) tape =
  caseM [
     (isIgnored meta <$> h     , diffNext),
     (isOpenBracket meta <$> h , skipRight meta tape >>= rcondiff meta s),
     ((x /=) <$> h             , return False)]
     diffNext
  where t' = moveRight tape `amendE` "rcondiff"
        h = (head . tapeHead) <$> t'
        diffNext = t' >>= rcondiff meta xs
  
-- For a string starting with a balanced delimiter, skip past the closing item.
skipRight :: (Eq a) => Metagrammar a -> Tape a -> TapeMonad a
skipRight meta tape
  | isAtEnd tape = throwE "Already at end in skipRight"
  | otherwise    = skipRightRec meta [(head . tapeHead) tape] tape

skipRightRec :: (Eq a) => Metagrammar a -> [a] -> Tape a -> TapeMonad a
skipRightRec _ [] xs = return xs
skipRightRec meta delimStack@(d:ds) tape
  | isAtEnd tape = throwE "skipRight: Missing closing delimiter."
  | closesBracket meta x d = tape' >>= skipRightRec meta ds
  | isOpenBracket meta x   = tape' >>= skipRightRec meta (x:ds)
  | otherwise              = tape' >>= skipRightRec meta delimStack
  where
    tape' = moveRight tape `amendE` "skipRightRec"
    x = (head . tapeHead) tape
