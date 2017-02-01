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
  opens,
  closes,
  module Error,
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
randomElement :: [(Double, String)] -> ErrorIO String
randomElement choices =
  if length choices == 1 then return $ snd $ head choices  -- conserve entropy
  else do
    x <- liftIO (getStdRandom (randomR (0.0, 1.0)) :: IO Double)
    let (_, s) = foldl (\(x', s') (p, s) ->
                          if x' <= 0 then (x', s')
                          else 
                           let x'' = x' - p in
                             if x'' <= 0 then (x'', Just s)
                             else (x'', Nothing)) (x, Nothing) choices
      in case s of
        Just s -> return s
        Nothing -> throwE' "Nothing in randomElement."


-- Read a number. If there is a decimal point without a leading digit,
-- insert one.
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


-- balancedSplit input at open punctuation ->
--   ErrorM (before punct., after punct.)
balancedSplit :: String -> ErrorM (String, String)
balancedSplit [] = return ("", "")
balancedSplit s@(x:xs) =
  if isOpenPunctuation x then do
    (result, remainder) <- balancedSplitRec [x] xs
    return (init result, remainder)
  else
    return ("", s)

balancedSplitRec :: String -> String -> ErrorM (String, String)
balancedSplitRec [] s = return (empty, s)
balancedSplitRec _ [] = throwE' "Missing close delimiter in balancedSplitRec."
balancedSplitRec stack (x:xs) =
  let newStack = case () of
        _ | x `closes` head stack -> tail stack
          | isOpenPunctuation x   -> x : stack
          | otherwise             -> stack
  in do
    (within, remaining) <- balancedSplitRec newStack xs
    return (x : within, remaining)

isOpenPunctuation :: Char -> Bool
isOpenPunctuation c = c == '(' || c == '[' || c == '{'

isClosePunctuation :: Char -> Bool
isClosePunctuation c = c == ')' || c == ']' || c == '}'

opens :: Char -> Char -> Bool
opens open close
  | open == '(' = close == ')'
  | open == '[' = close == ']'
  | open == '{' = close == '}'
  | otherwise = False

closes :: Char -> Char -> Bool
closes close open
  | open == '(' = close == ')'
  | open == '[' = close == ']'
  | open == '{' = close == '}'
  | otherwise = False
