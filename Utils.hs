module Utils (
  ErrorM(..),
  caseM,
  (&&&&), (||||), lNot,
  randomElement,
  readM,
  readDoubleM,
  stripSplit1,
  balancedSplit,
  module Error,
  module Control.Applicative,
  )
  where
import Error
import Control.Applicative
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

(&&&&) :: (Applicative m) => m Bool -> m Bool -> m Bool
(&&&&) = liftA2 (&&)

(||||) :: (Applicative m) => m Bool -> m Bool -> m Bool
(||||) = liftA2 (||)

lNot :: (Applicative m) => m Bool -> m Bool
lNot = liftA not

randomElement :: [a] -> ErrorIO a
randomElement a = do
  i <- liftIO $ getStdRandom (randomR(0, length a - 1))
  return $ a !! i

readM :: (Read a) => String -> ErrorM a
readM a@(a0:an) =
  let a' = case a0 of
        '.' -> "0" ++ a
        _   -> a
  in
  case maybeRead a' of
    Just b  -> return b
    Nothing -> throwE' (a ++ " can't be parsed as desired type.")

-- As I understand things, this case shouldn't need to be called out,
-- but without it, I can't parse fractional numbers.
readDoubleM :: String -> ErrorM Double
readDoubleM a@(a0:an) =
  let a' = case a0 of
        '.' -> "0" ++ a
        _   -> a
  in
  case maybeRead a' of
    Just b  -> return b
    Nothing -> throwE' ("\"" ++ a ++ "\" can't be parsed as a Double.")

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
    (result, remainder) <- balancedSplitRec [x] (trace ("splitting '" ++ s ++ "'") xs)
    return (init result, remainder)
  | otherwise = return ("", s)

balancedSplitRec :: String -> String -> ErrorM (String, String)
balancedSplitRec [] s = return ("", s)
balancedSplitRec (x:_) [] = throwE' "Missing close delimiter in balancedSplitRec."
balancedSplitRec delimStack@(d:ds) (x:xs) = do
  (within, remaining) <-
    if d `balances` x           then balancedSplitRec ds xs
    else if isOpenPunctuation x then balancedSplitRec (x:ds) xs
    else                             balancedSplitRec delimStack xs
  return (trace ("splitResult = " ++ show (x : within, remaining))
          (x : within, remaining))

isOpenPunctuation :: Char -> Bool
isOpenPunctuation c = c == '(' || c == '[' || c == '{'

balances :: Char -> Char -> Bool
balances open close
  | open == '(' = close == ')'
  | open == '[' = close == ']'
  | open == '{' = close == '}'
  | otherwise = False
