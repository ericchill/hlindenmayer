-- A tape like object with a read head between two lists.
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

module Tape (
  Tape(..),
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
  )
where
import Error
import Utils
import Data.List (foldl')

data Tape = Tape {
  tLeft  :: String,
  tRight :: String
  } deriving (Show)
  
type TapeMonad= ErrorM Tape

newTape :: String -> Tape
newTape = Tape []

isAtStart :: Tape -> Bool
isAtStart = null . tLeft

isAtEnd :: Tape -> Bool
isAtEnd = null . tRight

tapeHead :: Tape -> String
tapeHead = tRight

tapeHeadLeft :: Tape -> String
tapeHeadLeft = reverse . tLeft

rewind :: Tape -> Tape
rewind t = Tape [] $ tapeHeadLeft t ++ tRight t

moveRight :: Tape -> TapeMonad
moveRight t
  | isAtEnd t = throwE' "Tape already at right."
  | otherwise = return $ Tape (x:l) xs
  where l = tLeft t
        (x:xs) = tRight t

moveRightBy :: Int -> Tape -> TapeMonad
moveRightBy n t =
  let newLeft = foldl' (flip (:)) (tLeft t) $ take n $ tRight t
  in
    return $ Tape newLeft $ drop n $ tRight t

moveRightMatching :: String -> Tape-> TapeMonad
moveRightMatching x t =
  foldM (\t _ -> moveRight t) t $ takeWhile (uncurry (==)) $ zip x $ tRight t

moveLeft :: Tape -> TapeMonad
moveLeft t
  | isAtStart t = throwE' "Tape already at left."
  | otherwise = return $ Tape xs (x:r)
  where r = tRight t
        (x:xs) = tLeft t
