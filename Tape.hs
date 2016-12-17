module Tape where
import Data.List

-- First item is reverse of first half.
data Tape a = Tape {
  left :: [a],
  right  :: [a]
  } deriving (Eq)

instance (Show a) => Show (Tape a) where
  show (Tape l r) =
    (show $ reverse $ take 5 l) ++ " | " ++ (show $ reverse $ take 5 r)
newtype Productions a = Productions { runProductions :: a }

newTape :: Tape a
newTape = Tape [] []

atEnd :: Tape a -> Bool
atEnd (Tape _ []) = True
atEnd _              = False

atStart :: Tape a -> Bool
atStart (Tape [] _) = True
atStart _              = False

moveRight :: Tape a -> Tape a
moveRight (Tape l (x:xs)) = Tape (x:l) xs
moveRight (Tape _ []) = error "Tape already at right."

moveRightBy :: Tape a -> Int -> Tape a
moveRightBy tape dist = foldl' (\t _ -> moveRight t) tape ([1..dist] :: [Int])

moveLeft :: Tape a -> Tape a
moveLeft (Tape (x:xs) r) = Tape xs (x:r)
moveLeft (Tape [] _) = error "Tape already at left."

moveRightMatching :: (Eq a) => [a] -> Tape a -> Tape a
moveRightMatching [] a = a
moveRightMatching (x:xs) t@(Tape _ (a:_))
  | x == a = moveRightMatching xs $ moveRight t
  | otherwise = t

tapeHead :: Tape a -> [a]
tapeHead (Tape _ r) = r

tapeHeadLeft :: Tape a -> [a]
tapeHeadLeft (Tape l _) = reverse l

-- Place list onto tape, leaving head at first element
insertHead :: [a] -> Tape a -> Tape a
insertHead a (Tape l r) = Tape l (a ++ r)

-- Place list onto tape, leaving head just past last element
appendHead :: [a] -> Tape a -> Tape a
appendHead list (Tape left right) = Tape (foldr (\x ts -> x : ts) left list) right
