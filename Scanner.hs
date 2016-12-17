module Scanner where
import Tape
import Data.List

-- Call this with the match string reversed
lcondiff :: Eq a => ScanDelim a -> [a] -> [a] -> Tape a -> Bool
lcondiff _ _ [] _ = True
lcondiff delims ignore s@(x:xs) tape
  | (t `elem` ignore) || isOpenDelim delims t =
      lcondiff delims ignore s (moveLeft tape)
  | isCloseDelim delims t =
      lcondiff delims ignore s (skipLeft delims tape)
  | t == x =
      True
  | otherwise =
      lcondiff delims ignore xs (moveLeft tape)
  where t = head $ tapeHead tape

rcondiff :: Eq a => ScanDelim a -> [a] -> [a] -> Tape a -> Bool
rcondiff _ _ [] _ = True
rcondiff delims ignore s@(x:xs) tape
  | t `elem` ignore =
      rcondiff delims ignore s (moveRight tape)
  | isOpenDelim delims x =
      rcondiff delims ignore s (skipRight delims tape)
  | t == x =
      True
  | otherwise =
      rcondiff delims ignore xs (moveRight tape)
  where t = head $ tapeHead tape
  
-- For a string starting with a balanced delimiter, skip past the closing item.
skipRight :: Eq a => ScanDelim a -> Tape a -> Tape a
skipRight _ (Tape (_, [])) = error "Already at end in skipRight"
skipRight delims foo@(Tape (_, (x:_))) = skipRightRec delims foo [x]

skipRightRec :: Eq a => ScanDelim a -> Tape a -> [a] -> Tape a
skipRightRec _ xs [] = xs
skipRightRec _ (Tape (_, [])) (d:_) =
  error $ "skipRight: Missing closing delimiter."
skipRightRec delims state@(Tape (_, (x:_))) delimStack@(d:ds)
  | (isOpenDelim delims d) && (x == closeDelimFor delims d) =
      skipRightRec delims newState ds
  | isOpenDelim delims x =
      skipRightRec delims newState (x:ds)
  | otherwise =
    skipRightRec delims newState delimStack
  where
    newState = moveRight state
{-
  | d == '[' && x == ']' = 
  | d == '(' && x == ')' = skipRightRec newState ds
  | x == '('             = skipRightRec newState ds
  | x == '['             = skipRightRec newState (x:ds)
  | otherwise            = skipRightRec newState delimStack
-}
-- Leave head right before closing item
skipLeft :: Eq a => ScanDelim a -> Tape a -> Tape a
skipLeft delims (Tape (_, [])) = error "Already at end in skipLeft"
skipLeft delims foo@(Tape (_, (x:_))) = skipLeftRec delims foo [x]

skipLeftRec :: Eq a => ScanDelim a -> Tape a -> [a] -> Tape a
skipLeftRec delims xs [] = xs
skipLeftRec delims (Tape (_, [])) (d:_) = error $ "skipLeft: Missing opening delimiter."
skipLeftRec delims state@(Tape (_, (x:_))) delimStack@(d:ds)
  | (isCloseDelim delims d) && (x == openDelimFor delims d) =
      skipLeftRec delims newState ds
  | isCloseDelim delims x =
      skipLeftRec delims newState (x:ds)
  | otherwise =
      skipLeftRec delims newState delimStack
  where newState = moveLeft state

{-
  | d == ']' && x == '[' = skipLeftRec newState ds
  | d == ')' && x == '(' = skipLeftRec newState ds
  | x == ')'             = skipLeftRec newState (x:ds)
  | x == ']'             = skipLeftRec newState (x:ds)
  | otherwise            = skipLeftRec newState delimStack
-}


-- Opens and closes in same order
data ScanDelim a = ScanDelim [a] [a] deriving (Eq, Show)
isOpenDelim :: Eq a => ScanDelim a -> a -> Bool
isOpenDelim (ScanDelim opens _) a = a `elem` opens

isCloseDelim :: Eq a => ScanDelim a -> a -> Bool
isCloseDelim (ScanDelim _ closes) a = a `elem` closes

openDelims :: ScanDelim a -> [a]
openDelims (ScanDelim opens _) = opens

closeDelims :: ScanDelim a -> [a]
closeDelims (ScanDelim _ closes) = closes

openDelimFor :: Eq a => ScanDelim a -> a -> a
openDelimFor (ScanDelim opens closes) a =
  case elemIndex a closes of
    Just i  -> opens !! i
    Nothing -> error "No such delimiter."

closeDelimFor :: Eq a => ScanDelim a -> a -> a
closeDelimFor (ScanDelim opens closes) a =
  case elemIndex a opens of
    Just i  -> closes !! i
    Nothing -> error "No such delimiter."

