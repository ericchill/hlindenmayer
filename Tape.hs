-- A sequential object for reading before and after the current index.
{-# LANGUAGE MultiParamTypeClasses #-}
module Tape (
  Tape(..),
  tShow,
  TapeMonad,
  newTape,
  isAtEnd,
  isAtStart,
  rewind,
  moveRight,
  moveRightBy,
  moveRightMatching,
  moveLeft,
  tapeHead,
  tapeAtHead
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

isAtStart :: Tape -> Bool
isAtStart t = tIndex t == 0

isAtEnd :: Tape -> Bool
isAtEnd t = tIndex t == tMax t

tapeHead :: Tape -> String
tapeHead t = unsafeMiddleStr (tData t) (tIndex t)
--tapeHead t = T.unpack $ snd $ T.splitAt (tIndex t) (tData t)

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

moveRightBy :: Int -> Tape -> TapeMonad
moveRightBy n t = foldM (\t _ -> moveRight t) t [0..n-1]

moveRightMatching :: String -> Tape -> TapeMonad
moveRightMatching x t =
  foldM (\t _ -> moveRight t) t $
        takeWhile (uncurry (==)) $ zip x (tapeHead t)

moveLeft :: Tape -> TapeMonad
moveLeft t =
  if isAtStart t then throwE' "Tape already at left."
  else return $ t { tIndex = tIndex t - 1 }
