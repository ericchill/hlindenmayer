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

data Tape a = Tape {
  tLeft :: [a],
  tRight :: [a]
  } deriving (Show)
  
type TapeMonad a = ErrorM (Tape a)

newTape :: [a] -> Tape a
newTape = Tape []

isAtStart :: Tape a -> Bool
isAtStart = null . tLeft

isAtEnd :: Tape a -> Bool
isAtEnd = null . tRight

tapeHead :: Tape a -> [a]
tapeHead = tRight

tapeHeadLeft :: Tape a -> [a]
tapeHeadLeft = reverse . tLeft

rewind :: Tape a -> Tape a
rewind t = Tape [] $ tapeHeadLeft t ++ tRight t

moveRight :: (Show a) => Tape a -> TapeMonad a
moveRight t
  | isAtEnd t = throwE' "Tape already at right."
  | otherwise = return $ Tape (x:l) xs
  where l = tLeft t
        (x:xs) = tRight t

moveRightBy :: (Show a) => Int -> Tape a -> TapeMonad a
moveRightBy n t =
  let newLeft = foldl' (flip (:)) (tLeft t) $ take n $ tRight t
  in
    return $ Tape newLeft $ drop n $ tRight t
--  foldM (\t _ -> moveRight t)

moveRightMatching :: (Eq a, Show a) => [a] -> Tape a -> TapeMonad a
moveRightMatching x t =
  foldM (\t _ -> moveRight t) t $ takeWhile (uncurry (==)) $ zip x $ tRight t

moveLeft :: Tape a -> TapeMonad a
moveLeft t
  | isAtStart t = throwE' "Tape already at left."
  | otherwise = return $ Tape xs (x:r)
  where r = tRight t
        (x:xs) = tLeft t
