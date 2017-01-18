module Metagrammar (
  Metagrammar(..),
  mSetIgnore,
  testMeta,
  lcondiff,
  rcondiff,
  skipLeft,
  skipRight,
  skipAndCopy,
  Tape
    )
where
import Utils
import Tape
import Control.Applicative
import Control.Monad

data Metagrammar = Metagrammar {
  isOpenBracket  :: Char -> Bool,
  isCloseBracket :: Char -> Bool,
  closesBracket  :: Char -> Char -> Bool,
  isFuncArg      :: Char -> Bool,
  isBlank        :: Char -> Bool,
  isIgnored      :: Char -> Bool,
  isWild         :: Char -> Bool,
  isBreak        :: Char -> Bool,
  rsSig          :: String
  }

instance Eq Metagrammar where
  a == b = rsSig a == rsSig b

instance Show Metagrammar where
  show a = "(Metagrammar " ++ show (rsSig a) ++ ")"

mSetIgnore :: String -> Metagrammar -> Metagrammar
mSetIgnore a m = m { isIgnored = (`elem` a) }

testMeta :: Char -> Metagrammar
testMeta forType =
  let cf = const False
      cf2 = const (const False)
  in
  Metagrammar {
    isOpenBracket  = cf,
    isCloseBracket = cf,
    closesBracket  = cf2,
    isFuncArg      = cf,
    isBlank        = cf,
    isIgnored      = cf,
    isWild         = cf,
    isBreak        = cf,
    rsSig          = [forType]
    }

-- Check left context
lcondiff :: Metagrammar -> String -> Tape -> ErrorM Bool
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

-- Check right context
rcondiff :: Metagrammar -> String -> Tape -> ErrorM Bool
rcondiff _    []  _ = return True
rcondiff meta [x] _ = return $ isWild meta x
rcondiff meta s@(x:xs) t =
  caseM [
    (return $ isAtEnd t       , return $ isWild meta x),
    (isAtEnd <$> t'           , return False),
    (isIgnored meta <$> h     , diffNext),
    (isOpenBracket meta <$> h , do
        t' <- mapErrorM $ skipRight meta t
        rcondiff meta s t'),
    (return $ isWild meta x   , diffNext),
    ((x /=) <$> h             , return False)]
    diffNext
  where t' = moveRight t
        h = (head . tapeHead) <$> t'
        diffNext = t' >>= rcondiff meta xs
  
-- Leave head to left of item
skipLeft :: Metagrammar -> Tape -> TapeMonad
skipLeft meta t
  | isAtStart t = throwE' "Already at end in skipLeft"
  | otherwise   = skipLeftRec meta [(head . tapeHead) t] t

skipLeftRec :: Metagrammar -> String -> Tape -> TapeMonad
skipLeftRec _ [] t = return t
skipLeftRec meta delimStack@(d:ds) t
  | isAtStart t              = throwE' "skipLeft: Missing opening delimiter."
  | closesBracket meta d x   = t' >>= skipLeftRec meta ds
  | isCloseBracket meta x    = t' >>= skipLeftRec meta (x:ds)
  | otherwise                = t' >>= skipLeftRec meta delimStack
  where t' = moveLeft t `amendE'` "skipLeftRec"
        x = (head . tapeHead) t

-- Skip right, balancing parenthesis &c.
skipRight :: Metagrammar -> Tape -> TapeMonad
skipRight meta t
  | isAtEnd t  = throwE' "Already at end in skipAndCopy"
  | otherwise  = do
      t' <- moveRight t
      skipRightRec meta [(head . tapeHead) t] t'

skipRightRec :: Metagrammar -> String -> Tape -> TapeMonad
skipRightRec _ [] t = return t
skipRightRec meta stack t
  | isAtEnd t = throwE' "skipRight: Missing closing delimiter."
  | otherwise =
    let x = (head . tapeHead) t
        newStack = case x of
          _ | closesBracket meta x (head stack) -> tail stack
            | isOpenBracket meta x              -> x : stack
            | otherwise                         -> stack
    in do
      t' <- moveRight t `amendE'` "skipRight"
      skipRightRec meta newStack t'

-- For a string starting with a balanced delimiter, copy up to the left of
-- the close and return tape position to the right.
skipAndCopy :: Metagrammar -> Tape -> ErrorM (String, Tape)
skipAndCopy meta t
  | isAtEnd t  = throwE' "Already at end in skipAndCopy"
  | otherwise  = do
      t' <- moveRight t
      skipAndCopyRec meta [(head . tapeHead) t] t'

skipAndCopyRec :: Metagrammar -> String -> Tape -> ErrorM (String, Tape)
skipAndCopyRec _ [] t = return ([], t)
skipAndCopyRec meta stack t
  | isAtEnd t = throwE' "skipAndCopy: Missing closing delimiter."
  | otherwise =
    let x = (head . tapeHead) t
        newStack = case x of
          _ | closesBracket meta x (head stack) -> tail stack
            | isOpenBracket meta x              -> x : stack
            | otherwise                         -> stack
    in do
      t' <- moveRight t `amendE'` "skipAndCopy"
      (rest, t'') <- skipAndCopyRec meta newStack t'
      return (x : rest, t'')
