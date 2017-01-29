module Metagrammar (
  Metagrammar(..),
  mSetIgnore,
  testMeta,
  skipLeft,
  skipRight,
  skipAndCopy,
  skipAndCopyLeft,
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
  opensBracket   :: Char -> Char -> Bool,
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

testMeta :: Metagrammar
testMeta =
  let cf = const False
      cf2 = const (const False)
  in
  Metagrammar {
    isOpenBracket  = cf,
    isCloseBracket = cf,
    closesBracket  = cf2,
    opensBracket   = cf2,
    isFuncArg      = cf,
    isBlank        = cf,
    isIgnored      = cf,
    isWild         = cf,
    isBreak        = cf,
    rsSig          = "test"
    }

  
-- Leave head to left of item
skipLeft :: Metagrammar -> Tape -> TapeMonad
skipLeft meta t
  | isAtStart t = throwE' "Already at end in skipLeft"
  | otherwise   = skipLeftRec meta [tapeAtHead t] t

skipLeftRec :: Metagrammar -> String -> Tape -> TapeMonad
skipLeftRec _ [] t = return t
skipLeftRec meta delimStack@(d:ds) t
  | isAtStart t = throwE' "skipLeft: Missing opening delimiter."
  | otherwise = do
      t' <- moveLeft t `amendE'` "skipLeftRec"
      let x = tapeAtHead t in
        case () of
          _ | closesBracket meta d x -> skipLeftRec meta ds t'
            | isCloseBracket meta x  -> skipLeftRec meta (x:ds) t'
            | otherwise              -> skipLeftRec meta delimStack t'

-- Skip right, balancing parenthesis &c.
skipRight :: Metagrammar -> Tape -> TapeMonad
skipRight meta t
  | isAtEnd t  = throwE' "Already at end in skipRight"
  | otherwise  = do
      t' <- moveRight t
      skipRightRec meta [tapeAtHead t] t'

skipRightRec :: Metagrammar -> String -> Tape -> TapeMonad
skipRightRec _ [] t = return t
skipRightRec meta stack t
  | isAtEnd t = throwE' "skipRight: Missing closing delimiter."
  | otherwise =
      let x = tapeAtHead t
          newStack = case () of
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
      skipAndCopyRec meta [tapeAtHead t] t'

skipAndCopyRec :: Metagrammar -> String -> Tape -> ErrorM (String, Tape)
skipAndCopyRec _ [] t = return ([], t)
skipAndCopyRec meta stack t
  | isAtEnd t = throwE' "skipAndCopy: Missing closing delimiter."
  | otherwise =
      let x = tapeAtHead t
          newStack = case () of
            _ | closesBracket meta x (head stack) -> tail stack
              | isOpenBracket meta x              -> x : stack
              | otherwise                         -> stack
      in do
        t' <- moveRight t `amendE'` "skipAndCopy"
        (rest, t'') <- skipAndCopyRec meta newStack t'
        return (x : rest, t'')

-- For a string starting with a balanced delimiter, copy up to the right of
-- the open and return tape position to the left.
skipAndCopyLeft :: Metagrammar -> Tape -> ErrorM (String, Tape)
skipAndCopyLeft meta t
  | isAtStart t  = throwE' "Already at end in skipAndCopyLeft"
  | otherwise  = do
      t' <- moveLeft t
      skipAndCopyLeftRec meta [tapeAtHead t] t'

skipAndCopyLeftRec :: Metagrammar -> String -> Tape -> ErrorM (String, Tape)
skipAndCopyLeftRec _ [] t = return ([], t)
skipAndCopyLeftRec meta stack t
  | isAtStart t = throwE' "skipAndCopy: Missing closing delimiter."
  | otherwise =
      let x = tapeAtHead t
          newStack = case () of
            _ | opensBracket meta x (head stack) -> tail stack
              | isCloseBracket meta x            -> x : stack
              | otherwise                        -> stack
      in do
        t' <- moveLeft t `amendE'` "skipAndCopyLeft"
        (rest, t'') <- skipAndCopyLeftRec meta newStack t'
        return (x : rest, t'')
