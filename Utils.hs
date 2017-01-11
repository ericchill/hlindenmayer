module Utils (
  ErrorM(..),
  caseM,
  (&&&&), (||||), lNot,
  randomElement,
  readM,
  stripSplit1,
  balancedSplit,
  isOpenPunctuation,
  isClosePunctuation,
  closes,
  matchLongestPrefix,
  module Error,
  module Control.Applicative,
  )
  where
import Error
import Control.Applicative
import Data.Function (on)
import Data.List (isPrefixOf, sortBy)
import Data.List.Utils
import Data.String.Utils
import System.Random

-- A monadic case control structure
caseM :: [(ErrorM Bool, ErrorM a)] -> ErrorM a -> ErrorM a
caseM [] def = def
caseM (c:cs) def = do
  let (cond, action) = c
  test <- cond
  if test then action else caseM cs def

-- Applicative &&
(&&&&) :: (Applicative m) => m Bool -> m Bool -> m Bool
(&&&&) = liftA2 (&&)

-- Applictive ||
(||||) :: (Applicative m) => m Bool -> m Bool -> m Bool
(||||) = liftA2 (||)

-- Applicative not
lNot :: (Applicative m) => m Bool -> m Bool
lNot = liftA not


-- Return a random element from a list.
randomElement :: (Show a) => [a] -> ErrorIO a
randomElement a =
  if length a == 1 then return $ head a  -- conserve entropy
  else do
    i <- liftIO $ getStdRandom (randomR(0, length a - 1))
    return $! a !! i


-- Read a number. If there is a decimal point without a leading digit, insert one.
readM :: (Read a) => String -> ErrorM a
readM [] = throwE' "Can't make number out of empty string."
readM a@(a0:an) =
  let a' = case a0 of
        '.' -> "0" ++ a
        _   -> a
  in
  case maybeRead a' of
    Just b  -> return  b
    Nothing -> throwE' (a ++ " can't be parsed as desired type.")


-- Split a string at the delimiter and strip both halves of the result.
stripSplit1 :: String -> String -> (String, String)
stripSplit1 delim s =
  let (first, remainder) = breakList (startswith delim) s
  in
    (strip first, strip $ drop (length delim) remainder)


-- balancedSplit input at open punctuation -> ErrorM (before punct., after punct.)
balancedSplit :: String -> ErrorM (String, String)
balancedSplit [] = return ("", "")
balancedSplit s@(x:xs)
  | isOpenPunctuation x = do
    (result, remainder) <- balancedSplitRec [x] xs
    return $ result `seq` (init result, remainder)
  | otherwise = return ("", s)

balancedSplitRec :: String -> String -> ErrorM (String, String)
balancedSplitRec [] s = return ("", s)
balancedSplitRec (_:_) [] = throwE' "Missing close delimiter in balancedSplitRec."
balancedSplitRec stack (x:xs) =
  let newStack = case x of
        _ | x `closes` head stack -> tail stack
          | isOpenPunctuation x   -> x : stack
          | otherwise             -> stack
  in do
    (within, remaining) <-balancedSplitRec newStack xs
    return (x : within, remaining)

isOpenPunctuation :: Char -> Bool
isOpenPunctuation c = c == '(' || c == '[' || c == '{'

isClosePunctuation :: Char -> Bool
isClosePunctuation c = c == ')' || c == ']' || c == '}'

closes :: Char -> Char -> Bool
closes close open
  | open == '(' = close == ')'
  | open == '[' = close == ']'
  | open == '{' = close == '}'
  | otherwise = False


matchLongestPrefix :: (Eq a, Show a) => [a] -> [[a]] -> Maybe [a]
matchLongestPrefix s prefixes =
  case
       filter (`isPrefixOf` s) $ sortBy (compare `on` length) prefixes
  of
    [] -> Nothing
    x  -> Just $ head x
