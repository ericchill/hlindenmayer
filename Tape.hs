-- A sequential object for reading before and after the current index.
{-# LANGUAGE MultiParamTypeClasses #-}
module Tape (
  Tape(..)
  , tShow
  , TapeMonad
  , newTape
  , distance
  , isAtEnd
  , isAtStart
  , tapeHead
  , tapeAtHead
  , rewind
  , moveRight
  , moveLeft
  , moveRightBy
  , moveLeftBy
  , matchRight
  , matchLeft
  , skipLeft
  , skipRight
  , skipAndCopy
  , skipAndCopyLeft
  )
where
import Error
import Utils
import qualified Data.Text as T
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as TI
import qualified Data.Text.Internal.Unsafe.Char as TC

data Tape = Tape {
  tIndex :: Int,
  tMax   :: Int,
  tData  :: T.Text
  }

type TapeMonad = ErrorM Tape

tShow :: Tape -> String
tShow t = show $ snd $ T.splitAt (tIndex t) $ tData t

newTape :: String -> Tape
newTape x = Tape {
  tIndex = 0,
    tMax   = length x,
    tData  = T.pack x
  }

distance :: Tape -> Tape -> Int
distance (Tape i1 _ _) (Tape i2 _ _) = abs (i2 - i1)

isAtStart :: Tape -> Bool
isAtStart t = tIndex t == 0

isAtEnd :: Tape -> Bool
isAtEnd t = tIndex t == tMax t

tapeHead :: Tape -> String
tapeHead t = unsafeMiddleStr (tData t) (tIndex t)

-- 16x speed-up -- Will think about forking a variant of Data.Text
-- without all the Unicode stuff.
unsafeMiddleStr :: T.Text -> Int -> String
unsafeMiddleStr t@(TI.Text arr off len) i
  | i >= len = ""
  | otherwise = unsafeIndex t i : unsafeMiddleStr t (i + 1)

tapeAtHead :: Tape -> Char
tapeAtHead t = tData t `unsafeIndex` tIndex t

unsafeIndex :: T.Text -> Int -> Char
unsafeIndex (TI.Text arr off len) i =
  TC.unsafeChr $ A.unsafeIndex arr i

rewind :: Tape -> Tape
rewind t = t { tIndex = 0 }

moveRight :: Tape -> TapeMonad
moveRight t
  | isAtEnd t = throwE' "Tape already at right."
  | otherwise = return $ t { tIndex = tIndex t + 1 }

moveLeft :: Tape -> TapeMonad
moveLeft t =
  if isAtStart t then throwE' "Tape already at left."
  else return $ t { tIndex = tIndex t - 1 }

moveRightBy :: Int -> Tape -> TapeMonad
moveRightBy n t = foldM (\t _ -> moveRight t) t [0..n-1]

moveLeftBy :: Int -> Tape -> TapeMonad
moveLeftBy n t = foldM (\t _ -> moveLeft t) t [0..n-1]

moveRightMatching :: String -> Tape -> TapeMonad
moveRightMatching x t =
  foldM (\t _ -> moveRight t) t $
        takeWhile (uncurry (==)) $ zip x (tapeHead t)

matchRight :: String -> Tape -> ErrorM (Tape, Bool)
matchRight x t =
  let len = length x
  in
    if x == take len (tapeHead t) then do
      t' <- moveRightBy len t `amendE'` "matchRight"
      return (t', True)
    else
      return (t, False)

matchLeft :: String -> Tape -> ErrorM (Tape, Bool)
matchLeft x t =
  let len = length x
  in do {
    t' <- moveLeftBy len t;
    if x == take len (tapeHead t') then
      return (t', True)
    else
      return (t, False)
    } `catchE'` (\_ -> return (t, False))

-- Leave head to left of item
skipLeft :: Tape -> TapeMonad
skipLeft t
  | isAtStart t = throwE' "Already at end in skipLeft"
  | otherwise   = skipLeftRec [tapeAtHead t] t

skipLeftRec :: String -> Tape -> TapeMonad
skipLeftRec [] t = return t
skipLeftRec delimStack@(d:ds) t
  | isAtStart t = throwE' "skipLeft: Missing opening delimiter."
  | otherwise = do
      t' <- moveLeft t `amendE'` "skipLeftRec"
      let x = tapeAtHead t in
        case () of
          _ | closes d x           -> skipLeftRec ds t'
            | isClosePunctuation x -> skipLeftRec (x:ds) t'
            | otherwise            -> skipLeftRec delimStack t'

-- Skip right, balancing parenthesis &c.
skipRight :: Tape -> TapeMonad
skipRight t
  | isAtEnd t  = throwE' "Already at end in skipRight"
  | otherwise  = do
      t' <- moveRight t
      skipRightRec [tapeAtHead t] t'

skipRightRec :: String -> Tape -> TapeMonad
skipRightRec [] t = return t
skipRightRec stack t
  | isAtEnd t = throwE' "skipRight: Missing closing delimiter."
  | otherwise =
      let x = tapeAtHead t
          newStack = case () of
            _ | closes x (head stack) -> tail stack
              | isOpenPunctuation x   -> x : stack
              | otherwise             -> stack
      in do
        t' <- moveRight t `amendE'` "skipRight"
        skipRightRec newStack t'

-- For a string starting with a balanced delimiter, copy up to the left of
-- the close and return tape position to the right.
skipAndCopy :: Tape -> ErrorM (String, Tape)
skipAndCopy t
  | isAtEnd t  = throwE' "Already at end in skipAndCopy"
  | otherwise  = do
      t' <- moveRight t
      skipAndCopyRec [tapeAtHead t] t' `amendE'`
        ("skipping from " ++ tapeHead t')

skipAndCopyRec :: String -> Tape -> ErrorM (String, Tape)
skipAndCopyRec [] t = return ([], t)
skipAndCopyRec stack t
  | isAtEnd t = throwE' "skipAndCopy: Missing closing delimiter."
  | otherwise =
      let x = tapeAtHead t
          newStack = case () of
            _ | closes x (head stack) -> tail stack
              | isOpenPunctuation x   -> x : stack
              | otherwise             -> stack
      in do
        t' <- moveRight t `amendE'` "skipAndCopy"
        (rest, t'') <- skipAndCopyRec newStack t'
        return (x : rest, t'')

skipAndCopyLeft :: Tape -> ErrorM (String, Tape)
skipAndCopyLeft t
  | isAtStart t  = throwE' "Already at end in skipAndCopyLeft"
  | otherwise  = do
      t' <- moveLeft t
      skipAndCopyLeftRec [tapeAtHead t] t'

skipAndCopyLeftRec :: String -> Tape -> ErrorM (String, Tape)
skipAndCopyLeftRec [] t = return ([], t)
skipAndCopyLeftRec stack t
  | isAtStart t = throwE' "skipAndCopy: Missing closing delimiter."
  | otherwise =
      let x = tapeAtHead t
          newStack = case () of
            _ | opens x (head stack) -> tail stack
              | isClosePunctuation x -> x : stack
              | otherwise            -> stack
      in do
        t' <- moveLeft t `amendE'` "skipAndCopyLeft"
        (rest, t'') <- skipAndCopyLeftRec newStack t'
        return (x : rest, t'')
