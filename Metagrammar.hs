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
  isBreak        :: a -> Bool,
  nullSym        :: a,
  rsSig          :: [a]
  }

instance (Eq a) => Eq (Metagrammar a) where
  a == b = rsSig a == rsSig b

instance (Show a) => Show (Metagrammar a) where
  show a = "(Metagrammar " ++ show (rsSig a) ++ ")"

mSetIgnore :: (Eq a) => [a] -> Metagrammar a -> Metagrammar a
mSetIgnore a m = m { isIgnored = (`elem` a) }

testMeta :: a -> a -> Metagrammar a
testMeta nullEl forType =
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
    isBreak        = cf,
    nullSym       = nullEl,
    rsSig          = [forType]
    }

-- Check left context
lcondiff :: (Eq a, Show a) => Metagrammar a -> [a] -> Tape a -> ErrorM Bool
lcondiff _    []  _ = return True
lcondiff meta [x] _ = return $ isWild meta x
lcondiff meta s@(x:xs) t =
  caseM [
    (return $ isAtStart t     , return $ isWild meta x),
    (isAtStart <$> t'         , return False),
    (isIgnored meta <$> h     , diffNext),
    (isOpenBracket meta <$> h , diffNext),
    (isCloseBracket meta <$> h, skipLeft meta t >>= lcondiff meta s),
    (return $ isWild meta x   , diffNext),
    ((x /=) <$> h             , return False)]
    diffNext
  where  t' = moveLeft t 
         h = (head . tapeHead) <$> t'
         diffNext = join $ lcondiff meta xs <$> t'

-- Leave head right before closing item
skipLeft :: (Eq a) => Metagrammar a -> Tape a -> TapeMonad a
skipLeft meta t
  | isAtStart t = throwE' "Already at end in skipLeft"
  | otherwise   = skipLeftRec meta [(head . tapeHead) t] t

skipLeftRec :: (Eq a) => Metagrammar a -> [a] -> Tape a -> TapeMonad a
skipLeftRec _ [] t = return t
skipLeftRec meta delimStack@(d:ds) t
  | isAtStart t              = throwE' "skipLeft: Missing opening delimiter."
  | closesBracket meta d x   = t' >>= skipLeftRec meta ds
  | isCloseBracket meta x    = t' >>= skipLeftRec meta (x:ds)
  | otherwise                = t' >>= skipLeftRec meta delimStack
  where t' = moveLeft t `amendE'` "skipLeftRec"
        x = (head . tapeHead) t

-- Check right context
rcondiff :: (Eq a, Show a) => Metagrammar a -> [a] -> Tape a -> ErrorM Bool
rcondiff _    []  _ = return True
rcondiff meta [x] _ = return $ isWild meta x
rcondiff meta s@(x:xs) t =
  caseM [
    (return $ isAtEnd t       , return $ isWild meta x),
    (isAtEnd <$> t'           , return False),
    (isIgnored meta <$> h     , diffNext),
    (isOpenBracket meta <$> h , skipRight meta t >>= rcondiff meta s),
    (return $ isWild meta x   , diffNext),
    ((x /=) <$> h             , return False)]
    diffNext
  where t' = moveRight t
        h = (head . tapeHead) <$> t'
        diffNext = t' >>= rcondiff meta xs
  
-- For a string starting with a balanced delimiter, skip past the closing item.
skipRight :: (Eq a) => Metagrammar a -> Tape a -> TapeMonad a
skipRight meta t
  | isAtEnd t  = throwE' "Already at end in skipRight"
  | otherwise  = skipRightRec meta [(head . tapeHead) t] t

skipRightRec :: (Eq a) => Metagrammar a -> [a] -> Tape a -> TapeMonad a
skipRightRec _ [] t = return t
skipRightRec meta delimStack@(d:ds) t
  | isAtEnd t              = throwE' "skipRight: Missing closing delimiter."
  | closesBracket meta x d = t' >>= skipRightRec meta ds
  | isOpenBracket meta x   = t' >>= skipRightRec meta (x:ds)
  | otherwise              = t' >>= skipRightRec meta delimStack
  where
    t' = moveRight t `amendE'` "skipRightRec"
    x = (head . tapeHead) t
