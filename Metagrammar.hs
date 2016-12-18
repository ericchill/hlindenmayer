module Metagrammar (
  Metagrammar(..),
  lcondiff,
  rcondiff,
  skipLeft,
  skipRight,
  Tape
    )
where
import Tape

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

-- Call this with the match string reversed
lcondiff :: Eq a => Metagrammar a -> [a] -> Tape a -> Bool
lcondiff _ [] _ = True
lcondiff meta s@(x:xs) tape
  | (isIgnored meta) h      = justMove
  | (isOpenBracket meta) h  = justMove
  | (isCloseBracket meta) h = lcondiff meta s (skipLeft meta tape)
  | h /= x                  = False
  | otherwise               = justMove
  where  h = head $ tapeHead tape
         justMove = lcondiff meta xs (moveLeft tape)

rcondiff :: Eq a => Metagrammar a -> [a] -> Tape a -> Bool
rcondiff _ [] _ = True
rcondiff meta s@(x:xs) tape
  | (isIgnored meta) h     = justMove
  | (isOpenBracket meta) x = rcondiff meta s (skipRight meta tape)
  | h /= x                 = False
  | otherwise              = justMove
  where h = head $ tapeHead tape
        justMove = rcondiff meta xs (moveRight tape)
  
-- For a string starting with a balanced delimiter, skip past the closing item.
skipRight :: Eq a => Metagrammar a -> Tape a -> Tape a
skipRight _ (Tape _ []) = error "Already at end in skipRight"
skipRight meta tape = skipRightRec meta tape [(head . tapeHead) tape]

skipRightRec :: Eq a => Metagrammar a -> Tape a -> [a] -> Tape a
skipRightRec _ xs [] = xs
skipRightRec _ (Tape _ []) _ = error $ "skipRight: Missing closing delimiter."
skipRightRec meta tape delimStack@(d:ds)
  | (closesBracket meta) x d = skipRightRec meta tape' ds
  | (isOpenBracket meta) x   = skipRightRec meta tape' (x:ds)
  | otherwise                = skipRightRec meta tape' delimStack
  where
    tape' = moveRight tape
    x = (head . tapeHead) tape

-- Leave head right before closing item
skipLeft :: Eq a => Metagrammar a -> Tape a -> Tape a
skipLeft _ (Tape _ []) = error "Already at end in skipLeft"
skipLeft meta tape = skipLeftRec meta tape [(head . tapeHead) tape]  -- ???? off by one?

skipLeftRec :: Eq a => Metagrammar a -> Tape a -> [a] -> Tape a
skipLeftRec _ xs [] = xs
skipLeftRec _ (Tape _ []) _ = error $ "skipLeft: Missing opening delimiter."
skipLeftRec meta tape delimStack@(d:ds)
  | (closesBracket meta) d x = skipLeftRec meta tape' ds
  | (isCloseBracket meta) x  = skipLeftRec meta tape' (x:ds)
  | otherwise                = skipLeftRec meta tape' delimStack
  where tape' = moveLeft tape
        x = (head . tapeHead) tape

