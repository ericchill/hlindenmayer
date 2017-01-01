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

data Tape a = Tape {
  tLeft :: [a],
  tRight :: [a]
  }
  
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

mapRight :: (a -> a) -> Tape a -> Tape a
mapRight f t = t { tRight = map f $ tRight t }

moveRight :: Tape a -> TapeMonad a
moveRight t
  | isAtEnd t = throwError "Tape already at right."
  | otherwise = return $ Tape (x:l) xs
  where l = tLeft t
        (x:xs) = tRight t

moveRightBy :: Tape a -> Int -> TapeMonad a
moveRightBy tape dist = foldM (\t _ -> moveRight t) tape [1..dist]

moveRightMatching :: (Eq a) => [a] -> Tape a -> TapeMonad a
moveRightMatching x t =
  foldM (\t _ -> moveRight t) t $ takeWhile (uncurry (==)) $ zip x $ tRight t

moveLeft :: Tape a -> TapeMonad a
moveLeft t
  | isAtStart t = throwError "Tape already at left."
  | otherwise = return $ Tape xs (x:r)
  where r = tRight t
        (x:xs) = tLeft t

